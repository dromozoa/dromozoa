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
local compiler_error = require "dromozoa.compiler.compiler_error"
local lua54_regexp = require "dromozoa.compiler.lua54_regexp"
local lua54_parser = require "dromozoa.compiler.lua54_parser"
local generate = require "dromozoa.compiler.generate"

local quotes = {}
for byte = 0x00, 0x1F do
  quotes[string.char(byte)] = ([[\x%02X]]):format(byte)
end
quotes["\b"] = [[\b]]
quotes["\t"] = [[\t]]
quotes["\n"] = [[\n]]
quotes["\v"] = [[\v]]
quotes["\f"] = [[\f]]
quotes["\r"] = [[\r]]
quotes["\""] = [[\"]]
quotes["\\"] = [[\\]]

local LS = string.char(0xE2, 0x80, 0xA8)
local PS = string.char(0xE2, 0x80, 0xA9)

local function quote(s)
  return '"' .. s:gsub("[%z\1-\31\"\\]", quotes):gsub(LS, [[\u2028]]):gsub(PS, [[\u2029]]) .. '"'
end

local function append_mapping(source_map, u)
  if u then
    local file = source_map.files[u.f]
    if not file then
      file = append(source_map.files, u.f) - 1
      source_map.files[u.f] = file
    end
    append(source_map, { file = file, line = u.n - 1, column = u.c - 1 })
  else
    append(source_map, { file = 0, line = 0, column = 0 })
  end
end

local function generate_code(result, source_map, protos, u)
  local u_name = u[0]
  local a = u.a
  local b = u.b

  if false then

  elseif u_name == "break" then
    append(result, "break;")

  elseif u_name == "if" then
    append(result, "a=S.pop();if(a!==undefined&&a!==false){\n")
    append_mapping(source_map, u[1].node)
    for _, v in ipairs(u[1]) do
      generate_code(result, source_map, protos, v)
    end
    append(result, "}else{\n")
    append_mapping(source_map, u[2].node)
    for _, v in ipairs(u[2]) do
      generate_code(result, source_map, protos, v)
    end
    append(result, "}\n")
    append_mapping(source_map)
    return

  elseif u_name == "loop" then
    append(result, "while(true){\n")
    append_mapping(source_map, u.node)
    for _, v in ipairs(u) do
      generate_code(result, source_map, protos, v)
    end
    append(result, "}\n")
    append_mapping(source_map)
    return

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
  elseif u_name == "shr"    then append(result, "b=S.pop();a=S.pop();S.push(a>>b);")
  elseif u_name == "shl"    then append(result, "b=S.pop();a=S.pop();S.push(a<<b);")
  elseif u_name == "concat" then append(result, "b=S.pop();a=S.pop();S.push(a.toString()+b);")
  elseif u_name == "lt"     then append(result, "b=S.pop();a=S.pop();S.push(a<b);")
  elseif u_name == "le"     then append(result, "b=S.pop();a=S.pop();S.push(a<=b);")
  elseif u_name == "gt"     then append(result, "b=S.pop();a=S.pop();S.push(a>b);")
  elseif u_name == "ge"     then append(result, "b=S.pop();a=S.pop();S.push(a>=b);")
  elseif u_name == "eq"     then append(result, "b=S.pop();a=S.pop();S.push(a===b);")
  elseif u_name == "ne"     then append(result, "b=S.pop();a=S.pop();S.push(a!==b);")

  elseif u_name == "unm"  then append(result, "a=S.pop();S.push(-a);")
  elseif u_name == "not"  then append(result, "a=S.pop();S.push(a===undefined||a===false);")
  elseif u_name == "len"  then append(result, "a=S.pop();for(b=1;a.map.get(b)!==undefined;++b);S.push(b-1);")
  elseif u_name == "bnot" then append(result, "a=S.pop();S.push(~a);")

  elseif u_name == "new_local" then
    append(result, "V", a, "=[S.pop()];")

  elseif u_name == "set_local" then
    append(result, "V", a, "[0]=S.pop();")

  elseif u_name == "set_field" then
    append(result, "c=S.pop();b=S[", b - 1, "];a=S[", a - 1, "];if(a instanceof LuaTable)a.map.set(b,c);else a[b]=c;")

  elseif u_name == "set_table" then
    append(result, "c=S.pop();b=S.pop();a=S[", a - 1, "];if(a instanceof LuaTable)a.map.set(b,c);else a[b]=c;")

  elseif u_name == "get_local" then
    append(result, "S.push(V", a, "[0]);")

  elseif u_name == "get_upvalue" then
    append(result, "S.push(U", a, "[0]);")

  elseif u_name == "get_table" then
    append(result, "b=S.pop();a=S.pop();S.push(a instanceof LuaTable?a.map.get(b):a[b]);")

  elseif u_name == "new_table" then
    append(result, "S.push(new LuaTable());")

  elseif u_name == "closure" then
    append(result, "S.push(P", a, "(")
    for i, v in ipairs(protos[a].upvalues) do
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
    -- TODO hexadecimal floatをどうにかする
    append(result, "S.push(", a, ");")

  elseif u_name == "return" then
    append(result, "return S;")

  elseif u_name == "call" then
    append(result, "b=S.splice(", a, ");a=S.pop();if(a instanceof LuaFunction)b=a.fn(...b);else b=[a.apply(undefined,b)];")
    if b > 0 then
      append(result, "if(b.length<", b, ")b[", b - 1, "]=undefined;else b=b.splice(0,", b, ");")
    end
    if b ~= 0 then
      append(result, "S.push(...b);")
    end

  elseif u_name == "self" then
    append(result, "c=S.splice(", a + 1, ");b=S.pop();a=S.pop();b=a instanceof LuaTable?a.map.get(b):a[b];if(b instanceof LuaFunction)c=b.fn(a,...c);else c=[b.apply(a,c)];")
    if b > 0 then
      append(result, "if(c.length<", b, ")c[", b - 1, "]=undefined;else c=c.splice(0,", b, ");")
    end
    if b ~= 0 then
      append(result, "S.push(...c);")
    end

  elseif u_name == "set_list" then
    append(result, "b=S.splice(", a, ");a=S[", a - 1, "];for(c=0;c<b.length;++c)a.map.set(c+1,b[c]);")

  elseif u_name == "push_nil" then
    append(result, "S[S.length+", a - 1, "]=undefined;")

  elseif u_name == "pop" then
    append(result, "S.splice(-", a, ");")

  else
    append(result, "// NOT_IMPL ", u_name)
  end

  append(result, "\n")
  append_mapping(source_map, u.node)
end

local function generate_proto(result, source_map, protos, proto)
  append(result, "const P", proto.index, "=(")
  for i = 1, #proto.upvalues do
    if i > 1 then
      append(result, ",")
    end
    append(result, "U", i)
  end
  append(result, ")=>{\nreturn new LuaFunction((")
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
  append(result, ")=>{\nlet S=[],a,b,c;\n")
  append_mapping(source_map)
  append_mapping(source_map)
  append_mapping(source_map)

  for i = 1, #proto.locals do
    append(result, "let V", i)
    if i <= proto.nparams then
      append(result, "=[A", i, "]")
    end
    append(result, ";\n")
    append_mapping(source_map)
  end

  for _, v in ipairs(proto.code) do
    generate_code(result, source_map, protos, v)
  end

  append(result, "return S;\n});\n};\n")
  append_mapping(source_map)
  append_mapping(source_map)
  append_mapping(source_map)
end

local function generate_stage1(protos)
  local result = {}
  local source_map = { files = {} }

  append(result, [[
class LuaTable{constructor(){this.map=new Map();}}
class LuaFunction{constructor(fn){this.fn=fn;}}
]])
  append_mapping(source_map)
  append_mapping(source_map)

  for i = #protos, 1, -1 do
    generate_proto(result, source_map, protos, protos[i])
  end

  append(result, [=[
const env=new LuaTable();
env.map.set("globalThis",globalThis);
P1([env]).fn();
]=])
  append_mapping(source_map)
  append_mapping(source_map)
  append_mapping(source_map)

  return result, source_map
end

local source_filename, result_filename, source_map_filename = ...

local handle = assert(io.open(source_filename))
local source = handle:read "*a"
handle:close()

local parse = lua54_parser()
local root = lua54_regexp(source, source_filename, lua54_parser.max_terminal_symbol, parse)
local protos = generate(root)
local result, source_map = generate_stage1(protos)

local out = assert(io.open(result_filename, "w"))
out:write(table.concat(result), "//# sourceMappingURL=", source_map_filename, "\n")
out:close()

---------------------------------------------------------------------------

local base64_encoder = { [62] = "+", [63] = "/" }
for i = 0, 25 do
  base64_encoder[i] = string.char(string.byte "A" + i)
end
for i = 26, 51 do
  base64_encoder[i] = string.char(string.byte "a" + i - 26)
end
for i = 52, 61 do
  base64_encoder[i] = string.char(string.byte "0" + i - 52)
end

local function vlq(u)
  local result = {}

  if u < 0 then
    u = 1 - u * 2
  else
    u = u * 2
  end

  while true do
    local v = u % 0x20
    u = (u - v) / 0x20
    if u > 0 then
      append(result, base64_encoder[v + 0x20])
    else
      append(result, base64_encoder[v])
      break
    end
  end

  return table.concat(result)
end

local out = assert(io.open(source_map_filename, "w"))
out:write([[{"version":3,"file":]], quote(result_filename), [[,"sources":[]])
for i, file in ipairs(source_map.files) do
  if i > 1 then
    out:write ","
  end
  out:write(quote(file))
end
out:write [[],"names":[],"mappings":"]]

local prev_file = 0
local prev_line = 0
local prev_column = 0
for _, mapping in ipairs(source_map) do
  local file = mapping.file
  local line = mapping.line
  local column = mapping.column
  local f = file - prev_file
  local n = line - prev_line
  local c = column - prev_column
  if f == 0 and n == 0 and c == 0 then
    out:write(";")
  else
    out:write("A", vlq(f), vlq(n), vlq(c), ";")
    pref_file = file
    prev_line = line
    prev_column = column
  end
end

out:write '"}\n'
out:close()
