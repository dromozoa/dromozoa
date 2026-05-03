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

local source_location = require "dromozoa.source_location"

---@alias dromozoa.category "block" | "statement" | "expression" | "list" | "auxiliary"

---@class dromozoa.node
---@field category dromozoa.category
---@field kind string
---@field subkind string?
---@field token dromozoa.token?
---@field nodes dromozoa.node[]
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.node",
}

---@param category dromozoa.category
---@param kind string
---@param subkind string?
---@param token dromozoa.token?
---@return dromozoa.node
function class.new(category, kind, subkind, token)
  return setmetatable({
    category = category,
    kind = kind,
    subkind = subkind,
    token = token,
    nodes = {},
  }, metatable)
end

---@param node dromozoa.node
---@return dromozoa.node
function class:append(node)
  table.insert(self.nodes, node)
  return self
end

---@param nodes dromozoa.node[]
---@return dromozoa.node
function class:extend(nodes)
  for _, node in ipairs(nodes) do
    self:append(node)
  end
  return self
end

---@return dromozoa.source_location?
function class:srcloc()
  if self.token then
    return self.token.srcloc
  elseif #self.nodes > 0 then
    return self.nodes[1]:srcloc()
  else
    return nil
  end
end

---@param ... string
---@return boolean
function class:check(...kinds)
  for _, kind in ipairs(kinds) do
    if self.kind == kind then
      return true
    end
  end
  return false
end

---@param ... string
---@return dromozoa.node
function class:require(...)
  if self:check(...) then
    return self
  end
  error("syntax error at " .. source_location.to_string(self:srcloc()))
end

return class
