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
local token_stream = require "dromozoa.token_stream"

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

---@type string[]
local annotation_patterns = {}
for i, annotation in ipairs(annotations) do
  annotation_patterns[i] = annotation:gsub("%W", "%%%0")
end

-- https://github.com/LuaLS/lua-language-server/blob/master/script/parser/luadoc.lua
---@type string[]
local punctuators = {
  ":",
  "|",
  ",",
  ";", -- 未使用
  "<",
  ">",
  "(",
  ")",
  "?",
  "+", -- @castの型追加
  "#", -- コメント開始
  "{",
  "}",
  "*", -- 未使用
  "[]",
  "...",
  "[",
  "]",
  "-", -- @castの型除去
  ".", -- @versionの小数点
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

---@class dromozoa.annotation_lexer
---@field matcher dromozoa.matcher
---@field token_stream dromozoa.token_stream
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.annotation_lexer",
}

function class.new(matcher)
  local self = setmetatable({
    matcher = matcher,
  }, metatable)
  self.token_stream = token_stream.new(function()
    return self:lex()
  end)
  return self
end

---@param pattern string
---@return boolean
function class:match(pattern)
  return self.matcher:match(pattern)
end

---@return boolean
function class:lex_annotation()
  for _, pattern in ipairs(annotation_patterns) do
    if self:match(pattern) then
      return true
    end
  end
  return false
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

---@return dromozoa.token
function class:lex()
  local that = self.matcher
  local srcloc = that.srcloc:clone()

  ---@type string?
  local kind
  ---@type string?
  local subkind
  ---@type string?
  local value

  if self:match "%s+" then
    kind = "Space"
    value = that._0
  elseif self:match "`([^`]*)`" then
    kind = "Code"
    value = that._1
  elseif self:match "[%a_\x80-\xFF][%w_.*%-\x80-\xFF]*" then
    kind = "Name"
    value = that._0
  elseif self:lex_annotation() then
    kind = that._0
    value = that._0
  elseif self:lex_punctuator() then
    kind = that._0
    value = that._0
  end

  if not kind then
    return token.new("EOF", nil, "", "", srcloc)
  end

  local text = that.source:sub(srcloc.position, that.srcloc.position - 1)
  return token.new(kind, subkind, text, assert(value), srcloc)
end

---@return dromozoa.token
function class:peek()
  return self.token_stream:peek()
end

---@return dromozoa.token
function class:read()
  return self.token_stream:read()
end

function class:unread()
  self.token_stream:unread()
end

---@return integer
function class:tell()
  return self.token_stream:tell()
end

---@param index integer
function class:seek(index)
  self.token_stream:seek(index)
end

return class
