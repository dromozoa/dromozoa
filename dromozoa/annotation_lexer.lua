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
---@field source string
---@field srcloc dromozoa.source_location
---@field offset integer
---@field _0 string?
---@field _1 string?
---@field tokens dromozoa.token[]
---@field index integer
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.annotation_lexer",
}

---@param comment_token dromozoa.token
---@return dromozoa.annotation_lexer
function class.new(comment_token)
  local srcloc = comment_token.srcloc:clone()
  local self = setmetatable({
    source = comment_token.text,
    srcloc = srcloc,
    offset = srcloc.position - 1,
    _0 = nil,
    _1 = nil,
    tokens = {},
    index = 1,
  }, metatable)

  assert(comment_token:check "Comment")
  if comment_token.subkind == "Short" then
    assert(self:match "%-%-%-")
  elseif comment_token.subkind == "Long" then
    assert(self:match "%-%-%[(=*)%[")
  end

  return self
end

---@param pattern string
---@return boolean
function class:match(pattern)
  local i, j, value = self.source:find("^" .. pattern, self.srcloc.position - self.offset)
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
  local srcloc = self.srcloc:clone()

  ---@type string?
  local kind
  ---@type string?
  local subkind
  ---@type string?
  local value

  if self:match "%s+" then
    kind = "Space"
    value = self._0
  elseif self:match "`([^`]*)`" then
    kind = "Code"
    value = self._1
  elseif self:match "[%a_\x80-\xFF][%w_.*%-\x80-\xFF]*" then
    kind = "Name"
    value = self._0
  elseif self:lex_annotation() then
    kind = self._0
    value = self._0
  elseif self:lex_punctuator() then
    kind = self._0
    value = self._0
  end

  if not kind then
    return token.new("EOF", nil, "", "", srcloc)
  end

  local text = self.source:sub(srcloc.position, self.srcloc.position - 1)
  return token.new(kind, subkind, text, assert(value), srcloc)
end

---@return dromozoa.token
function class:peek()
  local n = #self.tokens

  for i = self.index, n do
    local token = self.tokens[i]
    if not token:check("Space", "Comment") then
      self.index = i
      return token
    end
  end

  if n > 0 and self.tokens[n]:check "EOF" then
    error "failed to peek"
  end

  local i = n + 1
  while true do
    local token = self:lex()
    self.tokens[i] = token
    if not token:check("Space", "Comment") then
      self.index = i
      return token
    end
    i = i + 1
  end
end

---@return dromozoa.token
function class:read()
  local token = self:peek()
  self.index = self.index + 1
  return token
end

function class:unread()
  for i = self.index - 1, 1, -1 do
    if not self.tokens[i]:check("Space", "Comment") then
      self.index = i
      return
    end
  end
  error "failed to unread"
end

---@return integer
function class:tell()
  return self.index
end

---@param index integer
function class:seek(index)
  self.index = index
end

return class
