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
table.sort(punctuators, matcher.longer_first)

---@param that dromozoa.matcher
---@return boolean
local function lex_punctuator(that)
  for _, punctuator in ipairs(punctuators) do
    if that:match(that.escape(punctuator)) then
      return true
    end
  end
  return false
end

---@param that dromozoa.matcher
---@return dromozoa.token
local function lex(that)
  local start_srcloc = that.start_srcloc
  ---@type string?
  local kind
  ---@type string?
  local subkind
  ---@type (string|number)?
  local value

  if that:is_at_start() and that:match "#([^\n]*)" then
    kind = "Comment"
    subkind = "Shebang"
    value = that._1
  elseif that:is_at_end() then
    kind = "EOF"
    value = ""
  elseif that:match "%s+" then
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
      error("unfinished long comment at " .. that.start_srcloc:to_string())
    end
    kind = "Comment"
    subkind = "Long"
    value = that._1
  elseif that:match "%-%-([^\n]*)" then
    kind = "Comment"
    subkind = "Short"
    value = that._1
  elseif that:match_long_string() then
    kind = "String"
    subkind = "Long"
    value = that._1
  elseif that:match_short_string() then
    kind = "String"
    subkind = "Short"
    value = that._1
  elseif lex_punctuator(that) then
    kind = that._0
    value = that._0
  end

  if not kind or not value then
    error("unexpected symbol at " .. start_srcloc:to_string())
  end

  return token.new(kind, subkind, that:substring(start_srcloc), value, start_srcloc, that.last_srcloc)
end

---@class dromozoa.lua_lexer
local class = {}

---@param that dromozoa.matcher
---@return dromozoa.token
function class.lex(that)
  return lex(that)
end

return class
