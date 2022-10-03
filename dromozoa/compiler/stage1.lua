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
local quote = require "dromozoa.compiler.quote"

local double_to_word
if string.pack then
  function double_to_word(v)
    return string.unpack("<I4I4", string.pack("<d", v))
  end
else
  function double_to_word(v)
    assert(v == v)
    if v == 0 then
      if 1 / v > 0 then
        return 0, 0
      else
        return 0, 0x80000000
      end
    elseif v == math.huge then
      return 0, 0x7FF00000
    elseif v == -math.huge then
      return 0, 0xFFF00000
    end

    local a -- 符号部  1bit
    local b -- 指数部 11bit
    local c -- 仮数部 20bit+32bit

    if v > 0 then
      a = 0
    else
      a = 0x80000000
      v = -v
    end

    local m, e = math.frexp(v)
    if e <= -1022 then
      b = 0
      c = math.ldexp(m, e + 1022)
    else
      b = e + 1022
      c = (m * 2 - 1)
    end
    local c, d = math.modf(c * 0x100000)

    return d * 0x100000000, a + b * 0x100000 + c
  end
end

local function generate_code(result, source_map, chunk, u)
  local u_name = u[0]
  local a = u.a
  local b = u.b

  if u_name == "break" then
    append(result, "break;")

  elseif u_name == "if" then
    append(result, "a=S.pop();if(a!==undefined&&a!==false){\n")
    source_map:append_mapping(u[1].node)
    for _, v in ipairs(u[1]) do
      generate_code(result, source_map, chunk, v)
    end
    append(result, "}else{\n")
    source_map:append_mapping(u[2].node)
    for _, v in ipairs(u[2]) do
      generate_code(result, source_map, chunk, v)
    end
    append(result, "}\n")
    source_map:append_empty_mappings(1)
    return

  elseif u_name == "loop" then
    append(result, "while(true){\n")
    source_map:append_mapping(u.node)
    for _, v in ipairs(u) do
      generate_code(result, source_map, chunk, v)
    end
    append(result, "}\n")
    source_map:append_empty_mappings(1)
    return

  elseif u_name == "check_for" then
    append(result, "// OP_CHECK_FOR ", a)

  elseif u_name == "add"    then append(result, "b=S.pop();a=S.pop();S.push(+a+ +b);")
  elseif u_name == "sub"    then append(result, "b=S.pop();a=S.pop();S.push(a-b);")
  elseif u_name == "mul"    then append(result, "b=S.pop();a=S.pop();S.push(a*b);")
  elseif u_name == "div"    then append(result, "b=S.pop();a=S.pop();S.push(a/b);")
  elseif u_name == "idiv"   then append(result, "b=S.pop();a=S.pop();S.push(Math.floor(a/b));")
  elseif u_name == "mod"    then append(result, "b=S.pop();a=S.pop();S.push((a%b+b)%b);")
  elseif u_name == "pow"    then append(result, "b=S.pop();a=S.pop();S.push(Math.pow(a,b));")
  elseif u_name == "band"   then append(result, "b=S.pop();a=S.pop();S.push(a&b);")
  elseif u_name == "bxor"   then append(result, "b=S.pop();a=S.pop();S.push(a^b);")
  elseif u_name == "bor"    then append(result, "b=S.pop();a=S.pop();S.push(a|b);")
  elseif u_name == "shr"    then append(result, "b=S.pop();a=S.pop();S.push(a>>>b);")
  elseif u_name == "shl"    then append(result, "b=S.pop();a=S.pop();S.push(a<<b);")
  elseif u_name == "concat" then append(result, "b=S.pop();a=S.pop();S.push(a.toString()+b.toString());")
  elseif u_name == "lt"     then append(result, "b=S.pop();a=S.pop();S.push(a<b);")
  elseif u_name == "le"     then append(result, "b=S.pop();a=S.pop();S.push(a<=b);")
  elseif u_name == "gt"     then append(result, "b=S.pop();a=S.pop();S.push(a>b);")
  elseif u_name == "ge"     then append(result, "b=S.pop();a=S.pop();S.push(a>=b);")
  elseif u_name == "eq"     then append(result, "b=S.pop();a=S.pop();S.push(a===b);")
  elseif u_name == "ne"     then append(result, "b=S.pop();a=S.pop();S.push(a!==b);")

  elseif u_name == "unm"  then append(result, "a=S.pop();S.push(-a);")
  elseif u_name == "not"  then append(result, "a=S.pop();S.push(a===undefined||a===false);")
  elseif u_name == "len"  then append(result, "a=S.pop();for(b=1;OP_GETTABLE(a,b)!==undefined;++b);S.push(b-1);")
  elseif u_name == "bnot" then append(result, "a=S.pop();S.push(~a);")

  elseif u_name == "new_local" or u_name == "tbc_local" then
    append(result, "V", a, "=[S.pop()];")

  elseif u_name == "set_local" then
    append(result, "V", a, "[0]=S.pop();")

  elseif u_name == "set_upvalue" then
    append(result, "U", a, "[0]=S.pop();")

  elseif u_name == "set_field" then
    append(result, "c=S.pop();b=S[", b - 1, "];a=S[", a - 1, "];OP_SETTABLE(a,b,c);")

  elseif u_name == "set_table" then
    append(result, "c=S.pop();b=S.pop();a=S[", a - 1, "];OP_SETTABLE(a,b,c);")

  elseif u_name == "get_local" then
    append(result, "S.push(V", a, "[0]);")

  elseif u_name == "get_upvalue" then
    append(result, "S.push(U", a, "[0]);")

  elseif u_name == "get_table" then
    append(result, "b=S.pop();a=S.pop();S.push(OP_GETTABLE(a,b));")

  elseif u_name == "new_table" then
    append(result, "S.push(new LuaTable());")

  elseif u_name == "closure" then
    append(result, "S.push(P", a, "(")
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
    append(result, "));")

  elseif u_name == "push_false" then
    append(result, "S.push(false);")

  elseif u_name == "push_true" then
    append(result, "S.push(true);")

  elseif u_name == "push_literal" then
    append(result, "S.push(", quote(a), ");")

  elseif u_name == "push_numeral" then
    if b == "HexadecimalFloatingNumeral" then
      append(result, ("S.push(new DataView(new Uint32Array([0x%X,0x%X]).buffer).getFloat64(0,true));"):format(double_to_word(tonumber(a))))
    else
      append(result, "S.push(", a, ");")
    end

  elseif u_name == "dup" then
    append(result, "S.push(S[S.length-1]);")

  elseif u_name == "close" then
    append(result, "a=V", a, "[0];if(a!==undefined)OP_CLOSE(a);V", a, "=undefined;")

  elseif u_name == "return" then
    append(result, "return S;")

  elseif u_name == "call" then
    append(result, "b=S.splice(", a, ");a=S.pop();b=OP_CALL(a,b);")
    if b ~= 0 then
      if b > 0 then
        append(result, "OP_ADJUST(b,", b, ");")
      end
      append(result, "S.push(...b);")
    end

  elseif u_name == "self" then
    append(result, "c=S.splice(", a + 1, ");b=S.pop();a=S.pop();c=OP_SELF(OP_GETTABLE(a,b),a,c);")
    if b ~= 0 then
      if b > 0 then
        append(result, "OP_ADJUST(c,", b, ");")
      end
      append(result, "S.push(...c);")
    end

  elseif u_name == "vararg" then
    if a > 0 then
      append(result, "a=[...VA];OP_ADJUST(a,", a, ");S.push(...a);")
    else
      append(result, "S.push(...VA);")
    end

  elseif u_name == "set_list" then
    append(result, "b=S.splice(", a, ");a=S[", a - 1, "];for(c=0;c<b.length;++c)OP_SETTABLE(a,c+1,b[c]);")

  elseif u_name == "push_nil" then
    if a == 1 then
      append(result, "S.push(undefined);")
    else
      append(result, "S[S.length+", a - 1, "]=undefined;")
    end

  elseif u_name == "pop" then
    if a == 1 then
      append(result, "S.pop();")
    else
      append(result, "S.splice(-", a, ");")
    end

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
  append(result, ")=>{\nlet S=[],a,b,c")
  for i, v in ipairs(proto.locals) do
    append(result, ",V", i)
    if i <= proto.nparams then
      append(result, "=[A", i, "]")
    end
    if v.attribute == "close" then
      try_catch = true
    end
  end
  append(result, ";\n")
  source_map:append_empty_mappings(2)

  if try_catch then
    append(result, "try{\n")
    source_map:append_empty_mappings(1)
  end

  for _, v in ipairs(proto.code) do
    generate_code(result, source_map, chunk, v)
  end

  if try_catch then
    append(result, "}catch(e){\n")
    source_map:append_empty_mappings(1)
    for i = #proto.locals, 1, -1 do
      local v = proto.locals[i]
      if v.attribute == "close" then
        append(result, "a=V", i, ";if(a!==undefined&&a[0]!==undefined)OP_CLOSE(a[0]);V", i, "=undefined;\n")
        source_map:append_empty_mappings(1)
      end
    end
    append(result, "throw e;}\n")
    source_map:append_empty_mappings(1)
  end

  append(result, "return S;});\n")
  source_map:append_empty_mappings(1)
end

local code, n = ([[
class LuaTable{constructor(){this.map=new Map();}}
class LuaFunction{constructor(fn){this.fn=fn;}}
class LuaError extends Error{constructor(msg){super(msg);this.name="LuaError";this.msg=msg}}
const D={
typeof:v=>typeof v,
is_table:v=>v instanceof LuaTable,
is_function:v=>v instanceof LuaFunction||v instanceof Function,
select_n:(...a)=>a.length,
error:msg=>{throw new LuaError(msg);},
getmetatable:t=>t.metatable,
setmetatable:(t,metatable)=>t.metatable=metatable,
newuserdata:(constructor,...args)=>new constructor(...args),
};
const OP_SETTABLE=(a,b,c)=>{if(a instanceof LuaTable)a.map.set(b,c);else a[b]=c;};
const OP_GETTABLE=(a,b)=>a instanceof LuaTable?a.map.get(b):a[b];
const OP_CALL=(a,b)=>a instanceof LuaFunction?a.fn(...b):a instanceof LuaTable?OP_CALL(OP_GETTABLE(a.metatable,"__call"),[a,...b]):[a.apply(undefined,b)];
const OP_SELF=(a,b,c)=>a instanceof LuaFunction||a instanceof LuaTable?OP_CALL(a,[b,...c]):[a.apply(b,c)];
const OP_CLOSE=a=>OP_CALL(OP_GETTABLE(a.metatable,"__close"));
const OP_ADJUST=(a,b)=>{if(a.length<b)a[b-1]=undefined;else a.splice(b);};
const env=new LuaTable();
OP_SETTABLE(env,"dromozoa",D);
OP_SETTABLE(env,"globalThis",globalThis);
const pkg=new LuaTable();
const preload=new LuaTable();
OP_SETTABLE(pkg,"preload",preload);
OP_SETTABLE(env,"package",pkg);
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

  append(result, "OP_CALL(P1([env]),[]);}\n")
  source_map:append_empty_mappings(1)
end

function module.generate_module(result, source_map, name, chunk)
  append(result, "{\n")
  source_map:append_empty_mappings(1)

  for i = #chunk, 1, -1 do
    generate_proto(result, source_map, chunk, chunk[i])
  end

  append(result, "OP_SETTABLE(preload,", quote(name), ",P1([env]));}\n")
  source_map:append_empty_mappings(1)
end

function module.generate_epilogue(result, source_map, source_map_filename)
  append(result, "//# sourceMappingURL=", source_map_filename, "\n")
  source_map:append_empty_mappings(1)
end

return module
