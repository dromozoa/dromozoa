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
  local that = matcher.new(source, filename)
  local srcloc = that.srcloc:clone()
  local result = {}

  if that:match "#(.-)\n" then
    table.insert(result, token.new("Comment", "Shebang", that._0, that._1, srcloc))
  end

  while not that:eof() do
    ---@type string?
    local kind
    ---@type string?
    local subkind
    ---@type (string|number)?
    local value

    srcloc = that.srcloc:clone()
    if that:match "%s+" then
      kind = "Space"
      value = that._0
    elseif that:match "0[xX]%x*%.%x+" or that:match "0[xX]%x+%." then
      local v = that._0
      if that:match "[pP][+%-]?%d+" then
        v = v .. that._0
      end
      kind = "Float"
      value = tonumber(v)
    elseif that:match "0[xX]%x+[pP][+%-]?%d+" then
      kind = "Float"
      value = tonumber(that._0)
    elseif that:match "%d*%.%d+" or that:match "%d+%." then
      local v = that._0
      if that:match "[eE][+%-]?%d+" then
        v = v .. that._0
      end
      kind = "Float"
      value = tonumber(v)
    elseif that:match "%d+[eE][+%-]?%d+" then
      kind = "Float"
      value = tonumber(that._0)
    elseif that:match "0[xX]%x+" or that:match "%d+" then
      kind = "Integer"
      value = tonumber(that._0)
    elseif that:match "[%a_][%w_]*" then
      if keyword_set[that._0] then
        kind = that._0
      else
        kind = "Name"
      end
      value = that._0
    elseif that:match "%-%-%[(=*)%[" then
      if not that:match("(.-)%]" .. that._1 .. "%]") then
        error("unfinished long comment at " .. srcloc:to_string())
      end
      kind = "Comment"
      subkind = "Long"
      value = that._1
    elseif that:match "%-%-(.-)\n" then
      kind = "Comment"
      subkind = "Short"
      value = that._1
    elseif that:match "%[(=*)%[" then
      if not that:match("\n?(.-)%]" .. that._1 .. "%]") then
        error("unfinished long string at " .. srcloc:to_string())
      end
      kind = "String"
      subkind = "Long"
      value = that._1
    elseif that:match "['\"]" then
      local quote = assert(that._0)
      local unescaped = "[^\\" .. quote .. "]+"
      kind = "String"
      subkind = "Short"
      value = ""
      while not that:match(quote) do
        if that:match(unescaped) then
          value = value .. that._0
        elseif that:match(escape_sequence_pattern) then
          value = value .. escape_sequences[that._1]
        elseif that:match "\\z%s*" then
          -- skip
        elseif that:match "\\x(%x%x)" then
          value = value .. string.char(tonumber(that._1, 16))
        elseif that:match "\\(%d%d?%d?)" then
          value = value .. string.char(tonumber(that._1, 10))
        elseif that:match "\\u{(%x+)}" then
          value = value .. utf8.char(tonumber(that._1, 16))
        else
          error("invalid escape sequence at " .. srcloc:to_string())
        end
      end
    else
      for _, pattern in ipairs(punctuator_patterns) do
        if that:match(pattern) then
          kind = that._0
          value = that._0
          break
        end
      end
      if not kind then
        error("unexpected symbol at " .. srcloc:to_string())
      end
    end

    local text = that.source:sub(srcloc.position, that.srcloc.position - 1)
    table.insert(result, token.new(assert(kind), subkind, text, assert(value), srcloc))
  end

  table.insert(result, token.new("EOF", nil, "", "", that.srcloc:clone()))
  return result
end

return class
