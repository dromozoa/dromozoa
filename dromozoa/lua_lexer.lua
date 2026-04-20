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

local keyword_set = {}
for _, keyword in ipairs(keywords) do
  keyword_set[keyword] = true
end

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
local punctuator_set = {}
for _, punctuator in ipairs(punctuators) do
  punctuator_max_length = math.max(punctuator_max_length, #punctuator)
  punctuator_set[punctuator] = true
end

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

---@class dromozoa.lua_lexer
---@field filename string
---@field source string
---@field srcloc dromozoa.source_location
---@field _0 string
---@field _1 string
---@field _2 string
---@field _3 string
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
    _2 = "",
    _3 = "",
  }, metatable)
end

---@param pattern string
---@result boolean
function class:match(pattern)
  local i, j, x, y, z = self.source:find("^" .. pattern, self.srcloc.position)
  if i then
    local text = self.source:sub(i, j)
    self.srcloc:update(text)
    self._0 = text
    self._1 = x
    self._2 = y
    self._3 = z
    return true
  else
    self._0 = ""
    self._1 = ""
    self._2 = ""
    self._3 = ""
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
      self._2 = ""
      self._3 = ""
      return true
    end
  end

  self._0 = ""
  self._1 = ""
  self._2 = ""
  self._3 = ""
  return false
end

---@return dromozoa.token[]
function class:lex()
  local result = {}

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
        elseif self:match "\\(.)" then
          local c = self._1
          if escape_sequences[c] then
            value = value .. escape_sequences[c]
          else
            error("lexer error at " .. srcloc:to_string())
          end
        else
          error("lexer error at " .. srcloc:to_string())
        end
      end
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
