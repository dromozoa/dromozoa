-- Copyright (C) 2026 Tomoyuki Fujimori <moyu@dromozoa.com>
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
-- along with dromozoa.  If not, see <https://www.gnu.org/licenses/>.

local lua_lex = require "dromozoa.lua_lex"
local lua_parser = require "dromozoa.lua_parser"
local matcher = require "dromozoa.matcher"
local source_location = require "dromozoa.source_location"
local token_stream = require "dromozoa.token_stream"
local util = require "dromozoa.util"

local escape_table = {
  ["&"] = "&amp;",
  ["<"] = "&lt;",
  [">"] = "&gt;",
  ["\""] = "&quot;",
  ["\'"] = "&apos;",
}

for byte = 0x00, 0x7F do
  local char = string.char(byte)
  if not escape_table[char] then
    escape_table[char] = ("&#x%X;"):format(byte)
  end
end

---@param source string
---@return string
local function escape(source)
  return (source:gsub("[\x00-\x08\x0B\x0C\x0E-\x1F\x7F&<>\"']", escape_table))
end

---@param token dromozoa.token
---@param depth integer
local function dump_token(token, depth)
  local indent = ("  "):rep(depth)
  io.write(indent, ('<token kind="%s"'):format(escape(token.kind)))
  if token.subkind then
    io.write((' subkind="%s"'):format(escape(token.subkind)))
  end
  io.write((' first_srcloc="%s"'):format(escape(token.first_srcloc:to_string())))
  io.write((' last_srcloc="%s"'):format(escape(token.last_srcloc:to_string())))
  io.write ">"
  io.write(escape(tostring(token.value)))
  io.write "</token>\n"
end

---@param u dromozoa.node
---@param depth integer
local function dump_node(u, depth)
  local indent = ("  "):rep(depth)
  depth = depth + 1

  io.write(indent, ('<node category="%s" kind="%s"'):format(escape(u.category), escape(u.kind)))
  if u.first_srcloc then
    io.write((' first_srcloc="%s"'):format(escape(u.first_srcloc:to_string())))
  end
  if u.last_srcloc then
    io.write((' last_srcloc="%s"'):format(escape(u.last_srcloc:to_string())))
  end
  io.write ">\n"

  if u.token then
    dump_token(u.token, depth)
  end

  if u.attribute then
    io.write(indent, "  <attribute>\n")
    dump_node(u.attribute, depth + 1)
    io.write(indent, "  </attribute>\n")
  end

  for _, v in ipairs(u.nodes) do
    dump_node(v, depth)
  end

  io.write(indent, "</node>\n")
end

local filename = ...
local source = util.normalize_eol(util.read_file(filename))
local chunk = lua_parser.new(token_stream.new(lua_lex, matcher.new(source, source_location.new(filename)))):parse()
dump_node(chunk, 0)
