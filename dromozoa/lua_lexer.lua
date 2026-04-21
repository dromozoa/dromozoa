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

local punctuator_max_length = 0
---@type table<string, boolean>
local punctuator_set = {}
for _, punctuator in ipairs(punctuators) do
  punctuator_max_length = math.max(punctuator_max_length, #punctuator)
  punctuator_set[punctuator] = true
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
}

local escape_sequence_pattern = "\\(["
for c in pairs(escape_sequences) do
  escape_sequence_pattern = escape_sequence_pattern .. c
end
escape_sequence_pattern = escape_sequence_pattern .. "])"

---@class dromozoa.lua_lexer
---@field filename string
---@field source string
---@field srcloc dromozoa.source_location
---@field _0 string
---@field _1 string
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
    _0 = "",
    _1 = "",
  }, metatable)
end

---@param pattern string
---@result boolean
function class:match(pattern)
  local i, j, x = self.source:find("^" .. pattern, self.srcloc.position)
  if i then
    local text = self.source:sub(i, j)
    self.srcloc:update(text)
    self._0 = text
    self._1 = x
    return true
  else
    self._0 = ""
    self._1 = ""
    return false
  end
end

---@result boolean
function class:punctuator()
  for n = punctuator_max_length, 1, -1 do
    local text = self.source:sub(self.srcloc.position, self.srcloc.position + n - 1)
    if punctuator_set[text] then
      self.srcloc:update(text)
      self._0 = text
      self._1 = ""
      return true
    end
  end

  self._0 = ""
  self._1 = ""
  return false
end

---@return dromozoa.token[]
function class:lex()
  local result = {}

  --TODO #!対応

  repeat
    local srcloc = self.srcloc:clone()
    local kind
    local value

    if self:match "%s+" then
      kind = "space"
    elseif self:match "%-%-%[(=*)%[" then
      if not self:match("(.-)%]" .. self._1 .. "%]") then
        error("lexer error at " .. srcloc:to_string())
      end
      kind = "comment"
      value = self._1
    elseif self:match "%-%-(.-)\n" then
      kind = "comment"
      value = self._1
    elseif self:match "[%a_][%w_]*" then
      value = self._0
      if keyword_set[value] then
        kind = value
      else
        kind = "name"
      end
    elseif self:punctuator() then
      kind = self._0
      value = kind
    elseif self:match "['\"]" then
      local quote = self._0
      local unescaped = "[^\\" .. quote .. "]+"

      kind = "string"
      value = ""
      while not self:match(quote) do
        if self:match(unescaped) then
          value = value .. self._0
        elseif self:match(escape_sequence_pattern) then
          value = value .. escape_sequences[self._1]
        elseif self:match "\\x(%x%x)" then
          value = value .. string.char(tonumber(self._1, 16))
        elseif self:match "\\(%d%d?%d?)" then
          value = value .. string.char(tonumber(self._1, 10))
        elseif self:match "\\u{(%x+)}" then
          value = value .. utf8.char(tonumber(self._1, 16))
        else
          error("lexer error at " .. srcloc:to_string())
        end
      end
    elseif self:match "%[(=*)%[" then
      if not self:match("\n?(.-)%]" .. self._1 .. "%]") then
        error("lexer error at " .. srcloc:to_string())
      end
      kind = "string"
      value = self._1
    elseif self:match "%d*%.%d+" or self:match "%d+%." then
      local v = self._0
      if self:match "[eE][+%-]?%d+" then
        v = v .. self._0
      end
      kind = "number"
      value = tonumber(v)
    elseif self:match "%d+[eE][+%-]?%d+" then
      kind = "number"
      value = tonumber(self._0)
    elseif self:match "0[xX]%x*%.%x+" or self:match "0[xX]%x+%." then
      local v = self._0
      if self:match "[pP][+%-]?%d+" then
        v = v .. self._0
      end
      kind = "number"
      value = tonumber(self._0)
    elseif self:match "0[xX]%x+[pP][+%-]?%d+" then
      kind = "number"
      value = tonumber(self._0)
    elseif self:match "0[xX]%x+" or self:match "%d+" then
      kind = "integer"
      value = tonumber(self._0)
    else
      error("lexer error at " .. srcloc:to_string())
    end

    local text = self.source:sub(srcloc.position, self.srcloc.position - 1)
    table.insert(result, token.new(kind, text, value and value or text, srcloc))
  until self.srcloc.position > #self.source

  table.insert(result, token.new("eof", "", "", self.srcloc:clone()))

  return result
end

return class
