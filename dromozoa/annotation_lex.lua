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

---@type string[]
local annotations = {
  "@alias",
  "@as",
  "@async",
  "@cast",
  "@class",
  "@deprecated",
  "@diagnostic",
  "@enum",
  "@field",
  "@generic",
  "@meta",
  "@module",
  "@nodiscard",
  "@operator",
  "@overload",
  "@package",
  "@param",
  "@private",
  "@protected",
  "@public",
  "@return",
  "@see",
  "@source",
  "@type",
  "@vararg",
  "@version",
}

---@param that dromozoa.matcher
---@return boolean
local function lex_annotation(that)
  for _, annotation in ipairs(annotations) do
    if that:match(that.escape(annotation)) then
      return true
    end
  end
  return false
end

---@type string[]
local punctuators = {
  ":",
  "|",
  ",",
  ";",
  "<",
  ">",
  "(",
  ")",
  "?",
  "+",
  "{",
  "}",
  "[]",
  "...",
  "[",
  "]",
  "-",
}

-- 最長一致させるために文字列長の降順で並びかえる。
table.sort(punctuators, function(a, b)
  if #a == #b then
    return a < b
  else
    return #a > #b
  end
end)

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
return function(that)
  local start_srcloc = that.start_srcloc
  ---@type string?
  local kind
  ---@type string?
  local subkind
  ---@type (string|number)?
  local value

  if that:is_at_start() and lex_annotation(that) then
    kind = that._0
    value = that._0
  elseif that:is_at_end() then
    kind = "EOF"
    value = ""
  elseif that:match "%s+" then
    kind = "Space"
    value = that._0
  elseif that:match "%d+%.[%w_.*%-\x80-\xFF]*" then
    kind = "Name"
    value = that._0
  elseif that:match "%-?%d+" then
    kind = "Integer"
    value = tonumber(that._0)
  elseif that:match "[%w_\x80-\xFF][%w_.*%-\x80-\xFF]*" then
    kind = "Name"
    value = that._0
  elseif that:match_long_string() then
    kind = "String"
    subkind = "Long"
    value = that._1
  elseif that:match_short_string() then
    kind = "String"
    subkind = "Short"
    value = that._1
  elseif that:match "`([^`]*)`" then
    kind = "Code"
    value = that._1
  elseif that:match "([@#])(.*)" then
    kind = "Comment"
    subkind = that._1
    value = that._2
  elseif that:match "%-%-(.*)" then
    kind = "Comment"
    subkind = "--"
    value = that._1
  elseif lex_punctuator(that) then
    kind = that._0
    value = that._0
  end

  if not kind or not value then
    kind = "EOF"
    value = ""
  end

  return token.new(kind, subkind, that:substring(start_srcloc), value, start_srcloc, that.last_srcloc)
end
