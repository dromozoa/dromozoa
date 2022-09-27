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

local function generate_code(result, protos, u)
  local u_name = u[0]
  local a = u.a
  local b = u.b

  if false then

  elseif u_name == "new_local" then
    append(result, "V", a, "=[S.pop()];\n")

  elseif u_name == "set_table" then
    append(result, "c=S.pop();b=S.pop();a=S[", a - 1, "];if(a instanceof Table)a.map.set(b,c);else a[b]=c;\n")

  elseif u_name == "get_local" then
    append(result, "S.push(V", a, "[0]);\n")

  elseif u_name == "get_upvalue" then
    append(result, "S.push(U", a, "[0]);\n")

  elseif u_name == "get_table" then
    append(result, "b=S.pop();a=S.pop();S.push(a instanceof Table?a.map.get(b):a[b]);\n")

  elseif u_name == "new_table" then
    append(result, "S.push(new Table());")

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
    append(result, "));\n")

  elseif u_name == "push_literal" then
    append(result, "S.push(", quote(a), ");\n")

  elseif u_name == "push_numeral" then
    -- TODO hexadecimal floatをどうにかする
    append(result, "S.push(", a, ");\n")

  elseif u_name == "return" then
    append(result, "return S;\n")

  elseif u_name == "call" then
    append(result, "b=S.splice(", a, ");a=S.pop();")
    if b ~= 0 then
      append(result,"c=")
    end
    append(result, "a(...b);")
    if b > 0 then
      append(result, "if(c.length<", b, ")c[", b - 1, "]=undefined;else c=c.splice(0,", b, ");")
    end
    if b ~= 0 then
      append(result, "S.push(...c);")
    end
    append(result, "\n")

  elseif u_name == "self" then
    append(result, "c=S.splice(", a + 1, ");b=S.pop();a=S.pop();")
    if b ~= 0 then
      append(result,"d=")
    end
    append(result, "(a instanceof Table?a.map.get(b):a[b]).apply(a,c);")
    if b > 0 then
      append(result, "if(d.length<", b, ")d[", b - 1, "]=undefined;else d=d.splice(0,", b, ");")
    end
    if b ~= 0 then
      append(result, "S.push(...d);")
    end
    append(result, "\n")

  else
    append(result, "//", u_name, "\n")
  end
end

local function generate_proto(result, protos, proto)
  append(result, "const P", proto.index, "=(")
  for i = 1, #proto.upvalues do
    if i > 1 then
      append(result, ",")
    end
    append(result, "U", i)
  end
  append(result, ")=>{\nreturn(")
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
  append(result, ")=>{\nlet S=[],a,b,c,d;\n")
  for i = 1, #proto.locals do
    append(result, "let V", i)
    if i <= proto.nparams then
      append(result, "=[A", i, "]")
    end
    append(result, ";\n")
  end

  for _, v in ipairs(proto.code) do
    generate_code(result, protos, v)
  end

  append(result, "};\n};\n")
end

local function generate_stage1(protos)
  local result = {}

  append(result, [[
class Table{
constructor(){
this.map=new Map();
}
};
]])

  for i = #protos, 1, -1 do
    generate_proto(result, protos, protos[i])
  end

  append(result, [=[
const env=new Table();
env.map.set("globalThis",globalThis);
P1([env])();
]=])

  return result
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
out:write(table.concat(result))
out:close()
