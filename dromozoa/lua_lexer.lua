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

local token = require "dromozoa.token"
local source_location = require "dromozoa.source_location"

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

---@class dromozoa.lua_lexer
---@field private filename string
---@field private source string
---@field private srcloc dromozoa.source_location
---@field private _0 string?
---@field private _1 string?
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.lua_lexer",
}

---@param filename string
---@param source string
---@return dromozoa.lua_lexer
function class.new(filename, source)
  return setmetatable({
    filename = filename,
    source = source,
    srcloc = source_location.new(filename),
    _0 = nil,
    _1 = nil,
  }, metatable)
end

---@private
---@param pattern string
---@result boolean
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

---@private
---@result boolean
function class:punctuator()
  for _, pattern in ipairs(punctuator_patterns) do
    if self:match(pattern) then
      return true
    end
  end
  return false
end

---@return dromozoa.token[]
function class:lex()
  local srcloc = self.srcloc:clone()
  local result = {}

  if self:match "#!(.-)\n" then
    table.insert(result, token.new("comment", self._1, self._0, srcloc))
  end

  repeat
    ---@type string?
    local kind
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
      value = self._1
    elseif self:match "%-%-(.-)\n" then
      kind = "Comment"
      value = self._1
    elseif self:match "%[(=*)%[" then
      if not self:match("\n?(.-)%]" .. self._1 .. "%]") then
        error("unfinished long string at " .. srcloc:to_string())
      end
      kind = "String"
      value = self._1
    elseif self:match "['\"]" then
      local quote = assert(self._0)
      local unescaped = "[^\\" .. quote .. "]+"
      kind = "String"
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
    elseif self:punctuator() then
      kind = self._0
      value = self._0
    else
      error("unexpected symbol at " .. srcloc:to_string())
    end

    local text = self.source:sub(srcloc.position, self.srcloc.position - 1)
    table.insert(result, token.new(assert(kind), text, assert(value), srcloc))
  until self.srcloc.position > #self.source

  table.insert(result, token.new("EOF", "", "", self.srcloc:clone()))

  return result
end

return class
