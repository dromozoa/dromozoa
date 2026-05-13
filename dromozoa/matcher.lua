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

---@class dromozoa.matcher
---@field source string
---@field start_offset integer
---@field start_srcloc dromozoa.source_location
---@field last_srcloc dromozoa.source_location
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
    last_srcloc = start_srcloc,
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

---@return boolean
function class:is_at_start()
  return self.start_srcloc.position - self.start_offset == 1
end

---@return boolean
function class:is_at_end()
  return self.start_srcloc.position - self.start_offset > #self.source
end

---@return dromozoa.source_location
function class:get_start_srcloc()
  return self.start_srcloc:clone()
end

---@param start_srcloc dromozoa.source_location
---@return string
function class:substring(start_srcloc)
  return self.source:sub(start_srcloc.position - self.start_offset, self.start_srcloc.position - self.start_offset - 1)
end

---@param pattern string
---@return boolean
function class:match(pattern)
  local i, j, u, v = self.source:find("^" .. pattern, self.start_srcloc.position - self.start_offset)
  if i then
    local text = self.source:sub(i, j)
    self.start_srcloc:update(text)
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
  local start_srcloc = self:get_start_srcloc()
  if self:match "%[(=*)%[" then
    if not self:match("\n?(.-)%]" .. self._1 .. "%]") then
      error("unfinished long string at " .. self.start_srcloc:to_string())
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
  local start_srcloc = self:get_start_srcloc()
  if self:match "['\"]" then
    local quote = self._0 --[[@as string]]
    local unescaped = "[^\\" .. quote .. "]+"
    local value = {}
    while not self:match(quote) do
      if self:match(unescaped) then
        table.insert(value, self._0)
      elseif self:match(escape_sequence_pattern) then
        table.insert(value, escape_sequences[self._1])
      elseif self:match "\\z%s*" then
        -- skip
      elseif self:match "\\x(%x%x)" then
        table.insert(value, string.char(tonumber(self._1, 16)))
      elseif self:match "\\(%d%d?%d?)" then
        table.insert(value, string.char(tonumber(self._1, 10)))
      elseif self:match "\\u{(%x+)}" then
        table.insert(value, utf8.char(tonumber(self._1, 16)))
      else
        error("invalid escape sequence at " .. self.start_srcloc:to_string())
      end
    end
    self._0 = self:substring(start_srcloc)
    self._1 = table.concat(value)
    self._2 = nil
    return true
  else
    return false
  end
end

return class
