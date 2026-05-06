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
  "@class",
  "@field",
  "@param",
  "@return",
  "@source",
  "@type",
}

---@type string[]
local annotation_patterns = {}
for i, annotation in ipairs(annotations) do
  annotation_patterns[i] = annotation:gsub("%W", "%%%0")
end

---@class dromozoa.annotation_lexer
---@field source string
---@field srcloc dromozoa.source_location
---@field offset integer
---@field _0 string?
---@field _1 string?
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
function class:eof()
  return self.srcloc.position > #self.source
end

function class:lex_annotation()
  for _, pattern in ipairs(annotation_patterns) do
    if self:match(pattern) then
      return true
    end
  end
  return false
end

function class:lex()
  local srcloc = self.srcloc:clone()

  if self:eof() then
    return token.new("EOF", nil, "", "", srcloc)
  end

  ---@type string?
  local kind
  ---@type string?
  local subkind
  ---@type string?
  local value

  if self:match "%s+" then
    kind = "Space"
    value = self._0
  elseif self:match "[%a_\x80-\xFF][%w_.*%-\x80-\xFF]*" then
    kind = "TypeName"
    value = self._0
  elseif self:lex_annotation() then
    kind = self._0
    value = self._0
  end

  if not kind then
    return token.new("EOF", nil, "", "", srcloc)
  end

  local text = self.source:sub(srcloc.position, self.srcloc.position - 1)
  return token.new(kind, subkind, text, assert(value), srcloc)
end

return class
