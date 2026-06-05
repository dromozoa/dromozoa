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
    escape_table[char] = string.format('<char byte="0x%02X"/>', byte)
  end
end

---@param s string
---@return string
local function escape(s)
  return (s:gsub("[\x00-\x08\x0B\x0C\x0E-\x1F\x7F&<>\"']", escape_table))
end

---@param u dromozoa.token
---@param depth integer
local function dump_token(u, depth)
  io.write(string.rep("  ", depth),
    string.format('<token kind="%s"%s first_srcloc="%s" last_srcloc="%s">%s</token>\n',
      escape(u.kind),
      u.subkind and string.format(' subkind="%s"', escape(u.subkind)) or "",
      escape(u.first_srcloc:to_string()),
      escape(u.last_srcloc:to_string()),
      escape(tostring(u.value))))
end

---@param u dromozoa.node
---@param depth integer
local function dump_node(u, depth)
  local indent = ("  "):rep(depth)
  depth = depth + 1

  io.write(indent,
    string.format('<node category="%s" kind="%s"%s%s>\n',
      escape(u.category),
      escape(u.kind),
      u.first_srcloc and string.format(' first_srcloc="%s"', escape(u.first_srcloc:to_string())) or "",
      u.last_srcloc and string.format(' last_srcloc="%s"', escape(u.last_srcloc:to_string())) or ""))

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
