-- Copyright (C) 2022 Tomoyuki Fujimori <moyu@dromozoa.com>
--
-- This file is part of dromozoa.
--
-- dromozoa is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- dromozoa is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with dromozoa.  If not, see <http://www.gnu.org/licenses/>.

local append = require "dromozoa.append"
local quote_js = require "dromozoa.quote_js"
local compiler_error = require "dromozoa.compiler.compiler_error"

local function boxed(v)
  return v.updef or v.def and v.upuse
end

local function box_env(chunk, value)
  if boxed(chunk.env) then
    return "[" .. value .. "]"
  else
    return value
  end
end

local function box_local(proto, var, value)
  if boxed(proto.locals[var]) then
    return "[" .. value .. "]"
  else
    return value
  end
end

local function unbox_local(proto, var)
  if boxed(proto.locals[var]) then
    return "V" .. var .. "[0]"
  else
    return "V" .. var
  end
end

local function unbox_upvalue(proto, var)
  if boxed(proto.upvalues[var].v) then
    return "U" .. var .. "[0]"
  else
    return "U" .. var
  end
end

local function pack_stack(m, n)
  local result = {}
  if n >= 0 then
    append(result, "[")
    for i = m, n do
      if i > m then
        append(result, ",")
      end
      append(result, "S", i)
    end
    append(result, "]")
  else
    local n = -n - 1
    if m > n then
      append(result, "R")
    else
      append(result, "[")
      for i = m, n do
        append(result, "S", i, ",")
      end
      append(result, "...R]")
    end
  end
  return table.concat(result)
end

local function unpack_stack(m, n, name)
  local result = {}
  for i = m, n do
    append(result, "S", i, "=", name, "[", i - m, "];")
  end
  return table.concat(result)
end

local function generate_code(result, source_map, chunk, proto, u)
  local u_name = u[0]
  local a = u.a
  local b = u.b
  local t = u.top

  if u_name == "break" then
    append(result, "break;")

  elseif u_name == "if" then
    append(result, "if(S", t, "!==undefined&&S", t, "!==false){\n")
    source_map:append_mapping(u[1].node)
    for _, v in ipairs(u[1]) do
      generate_code(result, source_map, chunk, proto, v)
    end
    append(result, "}else{\n")
    source_map:append_mapping(u[2].node)
    for _, v in ipairs(u[2]) do
      generate_code(result, source_map, chunk, proto, v)
    end
    append(result, "}\n")
    source_map:append_empty_mappings(1)
    return

  elseif u_name == "loop" then
    append(result, "while(true){\n")
    source_map:append_mapping(u.node)
    for _, v in ipairs(u) do
      generate_code(result, source_map, chunk, proto, v)
    end
    append(result, "}\n")
    source_map:append_empty_mappings(1)
    return

  elseif u_name == "check_for" then
    append(result, "V", a, "=D.checknumber(V", a, [[,"bad 'for' initial value");]])
    append(result, "V", a + 1, "=D.checknumber(V", a + 1, [[,"bad 'for' limit");]])
    append(result, "V", a + 2, "=D.checknumber(V", a + 2, [[,"bad 'for' step");]])
    append(result, "if(V", a + 2, [[===0)D.error("'for' step is zero");]])

  elseif u_name == "add"    then append(result, "S", t - 1, "=D.OP_ADD",    "(S", t - 1, ",S", t, ");")
  elseif u_name == "sub"    then append(result, "S", t - 1, "=D.OP_SUB",    "(S", t - 1, ",S", t, ");")
  elseif u_name == "mul"    then append(result, "S", t - 1, "=D.OP_MUL",    "(S", t - 1, ",S", t, ");")
  elseif u_name == "div"    then append(result, "S", t - 1, "=D.OP_DIV",    "(S", t - 1, ",S", t, ");")
  elseif u_name == "idiv"   then append(result, "S", t - 1, "=D.OP_IDIV",   "(S", t - 1, ",S", t, ");")
  elseif u_name == "mod"    then append(result, "S", t - 1, "=D.OP_MOD",    "(S", t - 1, ",S", t, ");")
  elseif u_name == "pow"    then append(result, "S", t - 1, "=D.OP_POW",    "(S", t - 1, ",S", t, ");")
  elseif u_name == "band"   then append(result, "S", t - 1, "=D.OP_BAND",   "(S", t - 1, ",S", t, ");")
  elseif u_name == "bxor"   then append(result, "S", t - 1, "=D.OP_BXOR",   "(S", t - 1, ",S", t, ");")
  elseif u_name == "bor"    then append(result, "S", t - 1, "=D.OP_BOR",    "(S", t - 1, ",S", t, ");")
  elseif u_name == "shr"    then append(result, "S", t - 1, "=D.OP_SHR",    "(S", t - 1, ",S", t, ");")
  elseif u_name == "shl"    then append(result, "S", t - 1, "=D.OP_SHL",    "(S", t - 1, ",S", t, ");")
  elseif u_name == "concat" then append(result, "S", t - 1, "=D.OP_CONCAT", "(S", t - 1, ",S", t, ");")
  elseif u_name == "lt"     then append(result, "S", t - 1, "=D.OP_LT",     "(S", t - 1, ",S", t, ");")
  elseif u_name == "le"     then append(result, "S", t - 1, "=D.OP_LE",     "(S", t - 1, ",S", t, ");")
  elseif u_name == "gt"     then append(result, "S", t - 1, "=D.OP_GT",     "(S", t - 1, ",S", t, ");")
  elseif u_name == "ge"     then append(result, "S", t - 1, "=D.OP_GE",     "(S", t - 1, ",S", t, ");")
  elseif u_name == "eq"     then append(result, "S", t - 1, "=D.OP_EQ",     "(S", t - 1, ",S", t, ");")
  elseif u_name == "ne"     then append(result, "S", t - 1, "=D.OP_NE",     "(S", t - 1, ",S", t, ");")

  elseif u_name == "unm"  then append(result, "S", t, "=D.OP_UNM",  "(S", t, ");")
  elseif u_name == "not"  then append(result, "S", t, "=D.OP_NOT",  "(S", t, ");")
  elseif u_name == "len"  then append(result, "S", t, "=D.OP_LEN",  "(S", t, ");")
  elseif u_name == "bnot" then append(result, "S", t, "=D.OP_BNOT", "(S", t, ");")

  elseif u_name == "new_local" or u_name == "tbc_local" then
    append(result, "V", a, "=", box_local(proto, a, "S" .. t), ";")

  elseif u_name == "set_local" then
    append(result, unbox_local(proto, a), "=S", t, ";")

  elseif u_name == "set_upvalue" then
    append(result, unbox_upvalue(proto, a), "=S", t, ";")

  elseif u_name == "set_field" then
    append(result, "D.OP_SETTABLE(S", a, ",S", b, ",S", t, ");")

  elseif u_name == "set_table" then
    append(result, "D.OP_SETTABLE(S", a, ",S", t - 1, ",S", t, ");")

  elseif u_name == "get_local" then
    append(result, "S", t + 1, "=", unbox_local(proto, a), ";")

  elseif u_name == "get_upvalue" then
    append(result, "S", t + 1, "=", unbox_upvalue(proto, a), ";")

  elseif u_name == "get_table" then
    append(result, "S", t - 1, "=D.OP_GETTABLE(S", t - 1, ",S", t, ");")

  elseif u_name == "new_table" then
    append(result, "S", t + 1, "=D.OP_NEWTABLE();")

  elseif u_name == "closure" then
    append(result, "S", t + 1, "=P", a, "(")
    for i, v in ipairs(chunk[a].upvalues) do
      if i > 1 then
        append(result, ",")
      end
      if v.var < 0 then
        append(result, "U", -v.var)
      else
        append(result, "V", v.var)
      end
    end
    append(result, ");")

  elseif u_name == "push_false" then
    append(result, "S", t + 1, "=false;")

  elseif u_name == "push_true" then
    append(result, "S", t + 1, "=true;")

  elseif u_name == "push_literal" then
    append(result, "S", t + 1, "=", quote_js(a), ";")

  elseif u_name == "push_numeral" then
    if b == "HexadecimalFloatingNumeral" then
      compiler_error("not supported: push_numeral " .. a .. " HexadecimalFloatingNumeral", u.node)
    else
      append(result, "S", t + 1, "=", a, ";")
    end

  elseif u_name == "dup" then
    append(result, "S", t + 1, "=S", t, ";")

  elseif u_name == "close" then
    append(result, "D.OP_CLOSE(", unbox_local(proto, a), ");V", a, "=undefined;")

  elseif u_name == "return" then
    append(result, "return ", pack_stack(1, t), ";")

  elseif u_name == "call" then
    if b ~= 0 then
      append(result, "R=")
    end
    append(result, "D.OP_CALL(S", a, ",", pack_stack(a + 1, t), ");")
    if b > 0 then
      append(result, unpack_stack(a, a + b - 1, "R"))
    end

  elseif u_name == "self" then
    if b ~= 0 then
      append(result, "R=")
    end
    append(result, "D.OP_SELF(D.OP_GETTABLE(S", a, ",S", a + 1, "),S", a, ",", pack_stack(a + 2, t), ");")
    if b > 0 then
      append(result, unpack_stack(a, a + b - 1, "R"))
    end

  elseif u_name == "vararg" then
    if a > 0 then
      append(result, unpack_stack(t + 1, t + a, "VA"))
    else
      assert(a == -1)
      append(result, "R=VA;")
    end

  elseif u_name == "set_list" then
    append(result, "D.OP_SETLIST(S", a, ",", pack_stack(a + 1, t), ");")

  elseif u_name == "push_nil" then
    for i = 1, a do
      append(result, "S", t + i, "=undefined;")
    end

  elseif u_name == "pop" then
    append(result, ";")

  else
    compiler_error("not supported: " .. u_name, u.node)
  end

  append(result, "\n")
  source_map:append_mapping(u.node)
end

local function generate_proto(result, source_map, chunk, proto)
  local try_catch

  append(result, "const P", proto.index, "=(")
  for i = 1, #proto.upvalues do
    if i > 1 then
      append(result, ",")
    end
    append(result, "U", i)
  end
  append(result, ")=>new LuaFunction((")
  for i = 1, proto.nparams do
    if i > 1 then
      append(result, ",")
    end
    append(result, "A", i)
  end
  if proto.vararg then
    if proto.nparams > 0 then
      append(result, ",")
    end
    append(result, "...VA")
  end
  append(result, ")=>{")
  if proto.node.f then
    append(result, "// ", proto.node.f, ":", proto.node.n, ":", proto.node.c)
  end
  append(result, "\n")
  source_map:append_empty_mappings(1)

  append(result, "let R,a,b,c")
  for i, v in ipairs(proto.locals) do
    append(result, ",V", i)
    if i <= proto.nparams then
      append(result, "=", box_local(proto, i, "A" .. i))
    end
    if v.attribute == "close" then
      try_catch = true
    end
  end
  for i = 1, proto.max do
    append(result, ",S", i)
  end
  append(result, ";\n")
  source_map:append_empty_mappings(1)

  if try_catch then
    append(result, "try{\n")
    source_map:append_empty_mappings(1)
  end

  for _, v in ipairs(proto.code) do
    generate_code(result, source_map, chunk, proto, v)
  end

  if try_catch then
    append(result, "}catch(e){\n")
    source_map:append_empty_mappings(1)
    for i = #proto.locals, 1, -1 do
      local v = proto.locals[i]
      if v.attribute == "close" then
        append(result, "if(V", i, "!==undefined)D.OP_CLOSE(", unbox_local(proto, i), ");V", i, "=undefined;\n")
        source_map:append_empty_mappings(1)
      end
    end
    append(result, "throw e;\n}\n")
    source_map:append_empty_mappings(2)
  end

  append(result, "return [];\n});\n")
  source_map:append_empty_mappings(2)
end

local code, n = ([[
import * as fs from "fs";
globalThis.fs=fs;
class LuaError extends Error{constructor(msg){super(msg);this.name="LuaError";this.msg=msg;}}
class LuaFunction{constructor(fn){this.fn=fn;}}
class LuaTable{constructor(){this.map=new Map();this.n=0}}
const D={
type_impl:{undefined:"nil",number:"number",string:"string",boolean:"boolean"},
type:a=>{const t=D.type_impl[typeof a];return t!==undefined?t:a instanceof LuaFunction?"function":a instanceof LuaTable?"table":"userdata";},
error:a=>{throw new LuaError(a);},
checknumber:(a,b)=>{const t=D.type(a),v=t==="number"||t==="string"?Number(a):NaN;if (Number.isNaN(v))D.error(b+" (number expected, got "+t+")");return v;},
rawget:(a,b)=>a instanceof LuaTable?a.map.get(b):a[b],
rawset:(a,b,c)=>{
  if(a instanceof LuaTable)if(c===undefined){if(a.n!==undefined&&a.n--!==b)a.n=undefined;a.map.delete(b);}else{if(a.n!==undefined&&++a.n!==b)a.n=undefined;a.map.set(b,c);}
  else if(c===undefined)delete a[b];else a[b]=c;
  return a;
},
rawlen:a=>{
  if(a instanceof LuaTable){let n=a.n;if(n!==undefined)return n;for(n=1;a.map.get(n)!==undefined;++n);return a.n=--n;}
  else return typeof a==="string"?D.string_len(a):a.length;
},
getmetatable:a=>{const m=typeof a==="string"?D.string_metatable:a.metatable;if(m===undefined)return m;const f=D.rawget(m,"__metatable");return f===undefined?m:f;},
getmetafield:(a,b)=>{const m=typeof a==="string"?D.string_metatable:a.metatable;if(m!==undefined)return D.rawget(m,b);},
setmetatable:(a,b)=>{if(D.getmetafield(a,"__metatable")!==undefined)D.error("cannot change a protected metatable");a.metatable=b;return a;},
select:new LuaFunction((a,...b)=>a==="#"?[b.length]:b.slice(a-1)),
native:(a)=>(...b)=>D.OP_CALL(a,b)[0],
array_pack:(...a)=>a,
array_unpack:new LuaFunction(a=>a),
array_from:(a,b,c)=>{let v=[];for(let i=b;i<=c;++i)v[i-b]=D.OP_GETTABLE(a,i);return v;},
newuserdata:(a,...b)=>new a(...b),
entries:a=>a.map.entries(),
replace:(a,...b)=>a.replace(...b),
arg:[],
OP_ADD:(a,b)=>Number(a)+Number(b),
OP_SUB:(a,b)=>a-b,
OP_MUL:(a,b)=>a*b,
OP_DIV:(a,b)=>a/b,
OP_IDIV:(a,b)=>Math.floor(a/b),
OP_MOD:(a,b)=>(a%b+b)%b,
OP_POW:(a,b)=>Math.pow(a,b),
OP_BAND:(a,b)=>a&b,
OP_BXOR:(a,b)=>a^b,
OP_BOR:(a,b)=>a|b,
OP_SHR:(a,b)=>a>>>b,
OP_SHL:(a,b)=>a<<b,
OP_CONCAT:(a,b)=>String(a)+String(b),
OP_LT:(a,b)=>a<b,
OP_LE:(a,b)=>a<=b,
OP_GT:(a,b)=>a>b,
OP_GE:(a,b)=>a>=b,
OP_EQ:(a,b)=>a===b,
OP_NE:(a,b)=>a!==b,
OP_UNM:a=>-a,
OP_NOT:a=>a===undefined||a===false,
OP_LEN:a=>D.rawlen(a),
OP_BNOT:a=>~a,
OP_SETTABLE:(a,b,c)=>{
  if(D.rawget(a,b)===undefined){const f=D.getmetafield(a,"__newindex");if(f!==undefined){if(f instanceof LuaTable)D.OP_SETTABLE(f,b,c);else D.OP_CALL(f,[a,b,c]);return a;}}
  return D.rawset(a,b,c);
},
OP_GETTABLE:(a,b)=>{
  if(typeof a!=="string"){const v=D.rawget(a,b);if(v!==undefined)return v;}
  const f=D.getmetafield(a,"__index");if(f!==undefined)return f instanceof LuaTable?D.OP_GETTABLE(f,b):D.OP_CALL(f,[a,b])[0];
},
OP_NEWTABLE:()=>new LuaTable(),
OP_CALL:(a,b)=>a instanceof LuaFunction?a.fn(...b):a instanceof LuaTable?D.OP_CALL(D.getmetafield(a,"__call"),[a,...b]):[a.apply(undefined,b)],
OP_SELF:(a,b,c)=>a instanceof LuaFunction?a.fn(b,...c):a instanceof LuaTable?D.OP_CALL(D.getmetafield(a,"__call"),[a,b,...c]):[a.apply(b,c)],
OP_CLOSE:(a)=>{if(a!==undefined)D.OP_CALL(D.getmetafield(a,"__close"),[a]);},
OP_ADJUST:(a,b)=>{if(a.length<b)a[b-1]=undefined;else a.splice(b);},
OP_SETLIST:(a,b)=>{for(let i=0;i<b.length;++i)D.rawset(a,i+1,b[i]);},
};
const E=new LuaTable();
D.OP_SETTABLE(E,"dromozoa",D);
D.OP_SETTABLE(E,"globalThis",globalThis);
D.OP_SETTABLE(E,"package",D.OP_SETTABLE(D.OP_SETTABLE(D.OP_NEWTABLE(),"preload",D.OP_NEWTABLE()),"loaded",D.OP_NEWTABLE()));
D.OP_SETTABLE(E,"type",D.type);
D.OP_SETTABLE(E,"error",D.error);
D.OP_SETTABLE(E,"getmetatable",D.getmetatable);
D.OP_SETTABLE(E,"setmetatable",D.setmetatable);
D.OP_SETTABLE(E,"select",D.select);
D.OP_SETTABLE(E,"pcall",new LuaFunction((a,...b)=>{try{return[true,...D.OP_CALL(a,b)];}catch(e){return[false,e instanceof LuaError?e.msg:e.toString()];}}));
]]):gsub("\n", {})

local module = {}

function module.generate_prologue(result, source_map)
  append(result, code)
  source_map:append_empty_mappings(n)
end

function module.generate_chunk(result, source_map, chunk)
  append(result, "{\n")
  source_map:append_empty_mappings(1)

  for i = #chunk, 1, -1 do
    generate_proto(result, source_map, chunk, chunk[i])
  end

  append(result, "D.OP_CALL(P1(", box_env(chunk, "E"), "),D.arg);\n}\n")
  source_map:append_empty_mappings(2)
end

function module.generate_module(result, source_map, name, chunk)
  append(result, "{\n")
  source_map:append_empty_mappings(1)

  for i = #chunk, 1, -1 do
    generate_proto(result, source_map, chunk, chunk[i])
  end

  append(result, 'D.OP_SETTABLE(D.OP_GETTABLE(D.OP_GETTABLE(E,"package"),"preload"),', quote_js(name), ",P1(", box_env(chunk, "E"), "));\n}\n")
  source_map:append_empty_mappings(2)
end

function module.generate_epilogue(result, source_map, source_map_filename)
  append(result, "//# sourceMappingURL=", source_map_filename, "\n")
  source_map:append_empty_mappings(1)
end

return module
