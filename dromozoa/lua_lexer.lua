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

local matcher = require "dromozoa.matcher"
local token = require "dromozoa.token"

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

---@param matcher dromozoa.matcher
---@return boolean
local function punctuator(matcher)
  for _, pattern in ipairs(punctuator_patterns) do
    if matcher:match(pattern) then
      return true
    end
  end
  return false
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
local class = {}

---@param source string
---@param filename string
---@return dromozoa.token[]
function class.lex(source, filename)
  local matcher = matcher.new(source, filename)
  local srcloc = matcher.srcloc:clone()
  local result = {}

  if matcher:match "#(.-)\n" then
    table.insert(result, token.new("Comment", "Shebang", matcher._0, matcher._1, srcloc))
  end

  while not matcher:eof() do
    ---@type string?
    local kind
    ---@type string?
    local subkind
    ---@type (string|number)?
    local value

    srcloc = matcher.srcloc:clone()
    if matcher:match "%s+" then
      kind = "Space"
      value = matcher._0
    elseif matcher:match "0[xX]%x*%.%x+" or matcher:match "0[xX]%x+%." then
      local v = matcher._0
      if matcher:match "[pP][+%-]?%d+" then
        v = v .. matcher._0
      end
      kind = "Float"
      value = tonumber(v)
    elseif matcher:match "0[xX]%x+[pP][+%-]?%d+" then
      kind = "Float"
      value = tonumber(matcher._0)
    elseif matcher:match "%d*%.%d+" or matcher:match "%d+%." then
      local v = matcher._0
      if matcher:match "[eE][+%-]?%d+" then
        v = v .. matcher._0
      end
      kind = "Float"
      value = tonumber(v)
    elseif matcher:match "%d+[eE][+%-]?%d+" then
      kind = "Float"
      value = tonumber(matcher._0)
    elseif matcher:match "0[xX]%x+" or matcher:match "%d+" then
      kind = "Integer"
      value = tonumber(matcher._0)
    elseif matcher:match "[%a_][%w_]*" then
      if keyword_set[matcher._0] then
        kind = matcher._0
      else
        kind = "Name"
      end
      value = matcher._0
    elseif matcher:match "%-%-%[(=*)%[" then
      if not matcher:match("(.-)%]" .. matcher._1 .. "%]") then
        error("unfinished long comment at " .. srcloc:to_string())
      end
      kind = "Comment"
      subkind = "Long"
      value = matcher._1
    elseif matcher:match "%-%-(.-)\n" then
      kind = "Comment"
      subkind = "Short"
      value = matcher._1
    elseif matcher:match "%[(=*)%[" then
      if not matcher:match("\n?(.-)%]" .. matcher._1 .. "%]") then
        error("unfinished long string at " .. srcloc:to_string())
      end
      kind = "String"
      subkind = "Long"
      value = matcher._1
    elseif matcher:match "['\"]" then
      local quote = assert(matcher._0)
      local unescaped = "[^\\" .. quote .. "]+"
      kind = "String"
      subkind = "Short"
      value = ""
      while not matcher:match(quote) do
        if matcher:match(unescaped) then
          value = value .. matcher._0
        elseif matcher:match(escape_sequence_pattern) then
          value = value .. escape_sequences[matcher._1]
        elseif matcher:match "\\z%s*" then
          -- skip
        elseif matcher:match "\\x(%x%x)" then
          value = value .. string.char(tonumber(matcher._1, 16))
        elseif matcher:match "\\(%d%d?%d?)" then
          value = value .. string.char(tonumber(matcher._1, 10))
        elseif matcher:match "\\u{(%x+)}" then
          value = value .. utf8.char(tonumber(matcher._1, 16))
        else
          error("invalid escape sequence at " .. srcloc:to_string())
        end
      end
    elseif punctuator(matcher) then
      kind = matcher._0
      value = matcher._0
    else
      if not kind then
        error("unexpected symbol at " .. srcloc:to_string())
      end
    end

    local text = matcher.source:sub(srcloc.position, matcher.srcloc.position - 1)
    table.insert(result, token.new(assert(kind), subkind, text, assert(value), srcloc))
  end

  table.insert(result, token.new("EOF", nil, "", "", matcher.srcloc:clone()))
  return result
end

return class
