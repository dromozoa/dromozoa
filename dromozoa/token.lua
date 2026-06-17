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

local node = require "dromozoa.node"

---@class dromozoa.token
---@field kind string
---@field subkind string?
---@field text string
---@field value string|number
---@field first_srcloc dromozoa.source_location
---@field last_srcloc dromozoa.source_location?
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.token",
}

---@param kind string
---@param subkind string?
---@param text string
---@param value string|number
---@param first_srcloc dromozoa.source_location
---@param last_srcloc dromozoa.source_location?
---@return dromozoa.token
function class.new(kind, subkind, text, value, first_srcloc, last_srcloc)
  -- EOFトークンはテキストが空で終了位置を持たない。
  if kind == "EOF" then
    assert(text == "")
    last_srcloc = nil
  else
    assert(text ~= "")
    assert(last_srcloc)
  end

  return setmetatable({
    kind = kind,
    subkind = subkind,
    text = text,
    value = value,
    first_srcloc = first_srcloc,
    last_srcloc = last_srcloc,
  }, metatable)
end

---@param ... string
---@return boolean
function class:check_kind(...kinds)
  for _, kind in ipairs(kinds) do
    if self.kind == kind then
      return true
    end
  end
  return false
end

---@param ... string
---@return dromozoa.token
function class:require_kind(...kinds)
  return self:require_kinds(kinds)
end

---@param kinds string[]
---@param message string?
---@return dromozoa.token
function class:require_kinds(kinds, message)
  if self:check_kind(table.unpack(kinds)) then
    return self
  end
  error((message or "unexpected symbol") .. " at " .. self.first_srcloc:to_string())
end

---@param kind string?
---@return dromozoa.node
function class:new_statement_node(kind)
  return node.new("statement", kind or self.kind, self)
end

---@param kind string?
---@return dromozoa.node
function class:new_expression_node(kind)
  return node.new("expression", kind or self.kind, self)
end

---@param kind string?
---@param attribute dromozoa.node?
---@return dromozoa.node
function class:new_auxiliary_node(kind, attribute)
  return node.new("auxiliary", kind or self.kind, self, attribute)
end

return class
