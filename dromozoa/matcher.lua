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

local source_location = require "dromozoa.source_location"

---@class dromozoa.matcher
---@field source string
---@field start_offset integer
---@field start_srcloc dromozoa.source_location
---@field last_srcloc dromozoa.source_location?
---@field _0 string?
---@field _1 string?
---@field _2 string?
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.matcher",
}

---@param source string
---@param start_srcloc dromozoa.source_location
---@return dromozoa.matcher
function class.new(source, start_srcloc)
  return setmetatable({
    source = source,
    start_offset = start_srcloc.position - 1,
    start_srcloc = start_srcloc,
    last_srcloc = nil,
    _0 = nil,
    _1 = nil,
    _2 = nil,
  }, metatable)
end

---@param source string
---@return string
function class.escape(source)
  return (source:gsub("%W", "%%%0"))
end

---@param a string
---@param b string
---@return boolean
function class.longer_first(a, b)
  if #a == #b then
    return a < b
  else
    return #a > #b
  end
end

---@param message string
---@param start_srcloc dromozoa.source_location
---@return string
function class.make_error_string(message, start_srcloc)
  return message .. start_srcloc:make_at_string()
end

---@return boolean
function class:is_at_start()
  return self.start_srcloc.position - self.start_offset == 1
end

---@return boolean
function class:is_at_end()
  return self.start_srcloc.position - self.start_offset > #self.source
end

---@param start_srcloc dromozoa.source_location
---@return string
function class:substring(start_srcloc)
  return self.source:sub(start_srcloc.position - self.start_offset, self.start_srcloc.position - self.start_offset - 1)
end

---@param text string
function class:update_srcloc(text)
  local n = #text
  local p = 1

  local start_line = self.start_srcloc.line
  local start_column = self.start_srcloc.column
  local last_line
  local last_column

  while true do
    local i = text:find("\n", p, true)
    if not i then
      break
    end

    last_line = start_line
    last_column = start_column + i - p

    p = i + 1
    start_line = start_line + 1
    start_column = 1
  end

  start_column = start_column + n - p + 1
  if start_column > 1 then
    last_line = start_line
    last_column = start_column - 1
  end

  local filename = self.start_srcloc.filename
  local position = self.start_srcloc.position + n

  self.start_srcloc = source_location.new(filename, position, start_line, start_column)
  self.last_srcloc = source_location.new(filename, position - 1, last_line, last_column)
end

---@param pattern string
---@return boolean
function class:match(pattern)
  local i, j, u, v = self.source:find("^" .. pattern, self.start_srcloc.position - self.start_offset)
  if i then
    local text = self.source:sub(i, j)
    self:update_srcloc(text)
    self._0 = text
    self._1 = u
    self._2 = v
    return true
  else
    self._0 = nil
    self._1 = nil
    self._2 = nil
    return false
  end
end

---@return boolean
function class:match_long_string()
  local start_srcloc = self.start_srcloc
  if self:match "%[%[" then
    if not self:match("\n?(.-)%]%]") then
      error(class.make_error_string("unfinished long string", self.start_srcloc))
    end
    self._0 = self:substring(start_srcloc)
    return true
  elseif self:match "%[(=+)" then
    local pattern = "\n?(.-)%]" .. self._1 .. "%]"
    if not self:match "%[" then
      error(class.make_error_string("invalid long string delimiter", self.start_srcloc))
    end
    if not self:match(pattern) then
      error(class.make_error_string("unfinished long string", self.start_srcloc))
    end
    self._0 = self:substring(start_srcloc)
    return true
  else
    return false
  end
end

---@type table<string, string>
local escape_sequences = {}

---@type string
local escape_sequence_pattern

do
  -- https://www.lua.org/manual/5.5/manual.html#3.1
  ---@type [string, string][]
  local rules = {
    { "a",  "\a" },
    { "b",  "\b" },
    { "f",  "\f" },
    { "n",  "\n" },
    { "r",  "\r" },
    { "t",  "\t" },
    { "v",  "\v" },
    { "\\", "\\" },
    { "\"", "\"" },
    { "\'", "\'" },
    { "\n", "\n" },
  }

  ---@type string[]
  local patterns = {}
  for _, rule in ipairs(rules) do
    local u, v = table.unpack(rule)
    escape_sequences[u] = v
    table.insert(patterns, class.escape(u))
  end
  escape_sequence_pattern = "\\([" .. table.concat(patterns) .. "])"
end

---@return boolean
function class:match_short_string()
  local start_srcloc = self.start_srcloc
  if not self:match "['\"]" then
    return false
  end

  ---@type string
  local quote = self._0
  local unescaped_pattern = "[^" .. quote .. "\\\n]+"
  local value = {}

  while true do
    local start_srcloc = self.start_srcloc
    if self:match(unescaped_pattern) then
      table.insert(value, self._0)
    elseif self:match(quote) then
      break
    elseif self:match(escape_sequence_pattern) then
      table.insert(value, escape_sequences[self._1])
    elseif self:match "\\z%s*" then
      -- skip
    elseif self:match "\\x" then
      if not self:match "%x%x" then
        error(class.make_error_string("hexadecimal digit expected", start_srcloc))
      end
      table.insert(value, string.char(tonumber(self._0, 16)))
    elseif self:match "\\(%d%d?%d?)" then
      local code = tonumber(self._1, 10)
      if code > 0xFF then
        error(class.make_error_string("decimal escape too large", start_srcloc))
      end
      table.insert(value, string.char(code))
    elseif self:match "\\u" then
      if not self:match "{" then
        error(class.make_error_string("missing '{'", start_srcloc))
      end
      if not self:match "%x" then
        error(class.make_error_string("hexadecimal digit expected", start_srcloc))
      end
      local code = tonumber(self._0, 16)
      while self:match "%x" do
        if code > 0x7FFFFFF then
          error(class.make_error_string("UTF-8 value too large", start_srcloc))
        end
        code = code << 4 | tonumber(self._0, 16)
      end
      if not self:match "}" then
        error(class.make_error_string("missing '}'", start_srcloc))
      end
      table.insert(value, utf8.char(code))
    elseif self:match "\\" then
      error(class.make_error_string("invalid escape sequence", start_srcloc))
    else
      error(class.make_error_string("unfinished string", start_srcloc))
    end
  end

  self._0 = self:substring(start_srcloc)
  self._1 = table.concat(value)
  self._2 = nil
  return true
end

return class
