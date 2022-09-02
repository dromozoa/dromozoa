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

local verbose = os.getenv "VERBOSE" == "1"

local dir = assert(...)

local lua54_regexp = require "dromozoa.compiler.lua54_regexp"
local lua54_parser = require "dromozoa.compiler.lua54_parser"
local generate = require "dromozoa.compiler.generate"
local generate_es = require "dromozoa.compiler.generate_es"

---------------------------------------------------------------------------

local quotes = {
  ['&'] = '&amp;';
  ['<'] = '&lt;';
  ['>'] = '&gt;';
  ['"'] = '&quot;';
}

local function quote(s)
  return '"' .. string.gsub(s, '[&<>"]', quotes) .. '"'
end

local function dump_attrs(out, u, attrs)
  for _, attr in ipairs(attrs) do
    local v = u[attr]
    if v ~= nil then
      local t = type(v)
      if t == "boolean" or t == "number" or t == "string" then
        out:write(" ", attr, "=", quote(tostring(v)))
      elseif #v > 0 then
        out:write(" ", attr, "=", quote(table.concat(v, ",")))
      end
    end
  end
end

local node_attrs = {
  "v";
  "declare", "resolve", "define", "label";
  "var", "env";
  "adjust", "nr";
  "loop";
  "self", "vararg";
  "attribute";
  "binop", "unop";
  "hint";
  "end_of_scope";
  "stack";
}

local function dump_node(out, u, n)
  if n == nil then
    n = 0
  else
    n = n + 1
  end

  out:write(("  "):rep(n), "<node")
  if u[0] ~= nil then
    out:write(" name=", quote(lua54_parser.symbol_names[u[0]]))
  end
  dump_attrs(out, u, node_attrs)

  if #u == 0 then
    out:write "/>\n"
  else
    out:write ">\n"
    for _, v in ipairs(u) do
      dump_node(out, v, n)
    end
    out:write(("  "):rep(n), "</node>\n")
  end
end

local function dump_proto_list(out, list, list_name, name)
  if #list == 0 then
    out:write("    <", list_name, "/>\n")
  else
    out:write("    <", list_name, ">\n")
    for i, v in ipairs(list) do
      out:write("      <", name, " index=\"", i, "\"")
      dump_attrs(out, v, { "name", "attribute", "var" })
      if v.node then
        dump_attrs(out, v.node, { "n", "c" })
      end
      out:write "/>\n"
    end
    out:write("    </", list_name, ">\n")
  end
end

local function dump_proto_code(out, u, n)
  if n == nil then
    n = 0
  else
    n = n + 1
  end

  out:write(("  "):rep(n), "<code op=", quote(u[0]))
  if u.a ~= nil then
    out:write(" a=", quote(u.a))
  end
  if u.b ~= nil then
    out:write(" b=", quote(u.b))
  end

  if #u == 0 then
    out:write "/>\n"
  else
    out:write ">\n"
    for _, v in ipairs(u) do
      dump_proto_code(out, v, n)
    end
    out:write(("  "):rep(n), "</code>\n")
  end
end

local function dump_protos(out, protos)
  out:write "<protos>\n"
  for _, proto in ipairs(protos) do
    out:write "  <proto"
    dump_attrs(out, proto, { "index", "nparams", "self", "vararg" })
    out:write ">\n"

    dump_proto_list(out, proto.locals, "locals", "local")
    dump_proto_list(out, proto.upvalues, "upvalues", "upvalue")
    dump_proto_list(out, proto.labels, "labels", "label")

    out:write "    <scopes>\n"
    for _, scope in ipairs(proto.scopes) do
      out:write "      <scope"
      dump_attrs(out, scope, { "index", "repeat_until", "labels", "locals" })
      out:write "/>\n"
    end
    out:write "    </scopes>\n"

    if #proto.code == 0 then
      out:write "    <codes/>\n"
    else
      out:write "    <codes>\n"
      for _, v in ipairs(proto.code) do
        dump_proto_code(out, v, 2)
      end
      out:write "    </codes>\n"
    end

    out:write "  </proto>\n"
  end
  out:write "</protos>\n"
end

for i = 2, #arg do
  local source_filename = assert(arg[i])
  local result_basename = assert(source_filename:match "([^/]+)%.lua$")
  result_basename = dir .. "/" .. result_basename

  local handle = assert(io.open(source_filename))
  local source = handle:read "*a"
  handle:close()

  local out = assert(io.open(result_basename .. "_list.xml", "w"))
  out:write "<nodes>\n"

  local parse = lua54_parser()
  local root = lua54_regexp(source, source_filename, lua54_parser.max_terminal_symbol, function (token)
    dump_node(out, token)
    return parse(token)
  end)

  out:write "</nodes>\n"
  out:close()

  local protos = generate(root)

  local out = assert(io.open(result_basename .. "_tree.xml", "w"))
  dump_node(out, root)
  out:close()

  local out = assert(io.open(result_basename .. "_protos.xml", "w"))
  dump_protos(out, protos)
  out:close()

  local out = assert(io.open(result_basename .. ".js", "w"))
  generate_es(out, protos)
  out:close()
end
