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
local token = require "dromozoa.token"

-- https://www.lua.org/manual/5.5/manual.html#3.1
---@type string[]
local keywords = {
  "and",
  "break",
  "do",
  "else",
  "elseif",
  "end",
  "false",
  "for",
  "function",
  "global",
  "goto",
  "if",
  "in",
  "local",
  "nil",
  "not",
  "or",
  "repeat",
  "return",
  "then",
  "true",
  "until",
  "while",
}

---@type table<string, boolean>
local keyword_set = {}
for _, keyword in ipairs(keywords) do
  keyword_set[keyword] = true
end

-- https://www.lua.org/manual/5.5/manual.html#3.1
---@type string[]
local punctuators = {
  "+",
  "-",
  "*",
  "/",
  "%",
  "^",
  "#",
  "&",
  "~",
  "|",
  "<<",
  ">>",
  "//",
  "==",
  "~=",
  "<=",
  ">=",
  "<",
  ">",
  "=",
  "(",
  ")",
  "{",
  "}",
  "[",
  "]",
  "::",
  ";",
  ":",
  ",",
  ".",
  "..",
  "...",
}

-- 最長一致させるために文字列長の降順で並びかえる。
table.sort(punctuators, function(a, b)
  if #a == #b then
    return a < b
  else
    return #a > #b
  end
end)

---@type string[]
local punctuator_patterns = {}
for i, punctuator in ipairs(punctuators) do
  punctuator_patterns[i] = punctuator:gsub("%W", "%%%0")
end

-- https://www.lua.org/manual/5.5/manual.html#3.1
---@type table<string, string>
local escape_sequences = {
  ["a"] = "\a",
  ["b"] = "\b",
  ["f"] = "\f",
  ["n"] = "\n",
  ["r"] = "\r",
  ["t"] = "\t",
  ["v"] = "\v",
  ["\\"] = "\\",
  ["\""] = "\"",
  ["\'"] = "\'",
  ["\n"] = "\n",
}

local escape_sequence_pattern = "\\(["
for char in pairs(escape_sequences) do
  escape_sequence_pattern = escape_sequence_pattern .. char:gsub("%W", "%%%0")
end
escape_sequence_pattern = escape_sequence_pattern .. "])"

---@class dromozoa.lexer
---@field source string?
---@field srcloc dromozoa.source_location?
---@field _1 string?
---@field _2 string?
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.lexer",
}

---@return dromozoa.lexer
function class.new()
  return setmetatable({
    source = nil,
    srcloc = nil,
    _1 = nil,
    _2 = nil,
  }, metatable)
end

---@param pattern string
---@return boolean
function class:match(pattern)
  local i, j, value = self.source:find("^" .. pattern, self.srcloc.position)
  if i then
    local text = self.source:sub(i, j)
    self.srcloc:update(text)
    self._0 = text
    self._1 = value
    return true
  end
  self._0 = nil
  self._1 = nil
  return false
end

---@return boolean
function class:eof()
  return self.srcloc.position > #self.source
end

---@return boolean
function class:lex_punctuator()
  for _, pattern in ipairs(punctuator_patterns) do
    if self:match(pattern) then
      return true
    end
  end
  return false
end

---@param source string
---@param filename string
---@return dromozoa.token[]
function class:lex(source, filename)
  self.source = source
  self.srcloc = source_location.new(filename)
  self._1 = nil
  self._2 = nil

  local srcloc = self.srcloc:clone()
  local result = {}

  if self:match "#(.-)\n" then
    table.insert(result, token.new("Comment", "Shebang", self._0, self._1, srcloc))
  end

  while not self:eof() do
    ---@type string?
    local kind
    ---@type string?
    local subkind
    ---@type (string|number)?
    local value

    srcloc = self.srcloc:clone()
    if self:match "%s+" then
      kind = "Space"
      value = self._0
    elseif self:match "0[xX]%x*%.%x+" or self:match "0[xX]%x+%." then
      local v = self._0
      if self:match "[pP][+%-]?%d+" then
        v = v .. self._0
      end
      kind = "Float"
      value = tonumber(v)
    elseif self:match "0[xX]%x+[pP][+%-]?%d+" then
      kind = "Float"
      value = tonumber(self._0)
    elseif self:match "%d*%.%d+" or self:match "%d+%." then
      local v = self._0
      if self:match "[eE][+%-]?%d+" then
        v = v .. self._0
      end
      kind = "Float"
      value = tonumber(v)
    elseif self:match "%d+[eE][+%-]?%d+" then
      kind = "Float"
      value = tonumber(self._0)
    elseif self:match "0[xX]%x+" or self:match "%d+" then
      kind = "Integer"
      value = tonumber(self._0)
    elseif self:match "[%a_][%w_]*" then
      if keyword_set[self._0] then
        kind = self._0
      else
        kind = "Name"
      end
      value = self._0
    elseif self:match "%-%-%[(=*)%[" then
      if not self:match("(.-)%]" .. self._1 .. "%]") then
        error("unfinished long comment at " .. srcloc:to_string())
      end
      kind = "Comment"
      subkind = "Long"
      value = self._1
    elseif self:match "%-%-(.-)\n" then
      kind = "Comment"
      subkind = "Short"
      value = self._1
    elseif self:match "%[(=*)%[" then
      if not self:match("\n?(.-)%]" .. self._1 .. "%]") then
        error("unfinished long string at " .. srcloc:to_string())
      end
      kind = "String"
      subkind = "Long"
      value = self._1
    elseif self:match "['\"]" then
      local quote = assert(self._0)
      local unescaped = "[^\\" .. quote .. "]+"
      kind = "String"
      subkind = "Short"
      value = ""
      while not self:match(quote) do
        if self:match(unescaped) then
          value = value .. self._0
        elseif self:match(escape_sequence_pattern) then
          value = value .. escape_sequences[self._1]
        elseif self:match "\\z%s*" then
          -- skip
        elseif self:match "\\x(%x%x)" then
          value = value .. string.char(tonumber(self._1, 16))
        elseif self:match "\\(%d%d?%d?)" then
          value = value .. string.char(tonumber(self._1, 10))
        elseif self:match "\\u{(%x+)}" then
          value = value .. utf8.char(tonumber(self._1, 16))
        else
          error("invalid escape sequence at " .. srcloc:to_string())
        end
      end
    elseif self:lex_punctuator() then
      kind = self._0
      value = self._0
    else
      if not kind then
        error("unexpected symbol at " .. srcloc:to_string())
      end
    end

    local text = self.source:sub(srcloc.position, self.srcloc.position - 1)
    table.insert(result, token.new(assert(kind), subkind, text, assert(value), srcloc))
  end

  table.insert(result, token.new("EOF", nil, "", "", self.srcloc:clone()))
  return result
end

return class
