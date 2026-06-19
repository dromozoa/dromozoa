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

---@alias dromozoa.node.category "block" | "statement" | "expression" | "auxiliary"

---@class dromozoa.node
---@field category dromozoa.node.category
---@field kind string
---@field token dromozoa.token?
---@field attribute dromozoa.node?
---@field nodes dromozoa.node[]
---@field first_srcloc? dromozoa.source_location
---@field last_srcloc? dromozoa.source_location
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.node",
}

---@param category dromozoa.node.category
---@param kind string
---@param token dromozoa.token?
---@param attribute dromozoa.node?
---@return dromozoa.node
function class.new(category, kind, token, attribute)
  local self = setmetatable({
    category = category,
    kind = kind,
    token = token,
    attribute = attribute,
    nodes = {},
    first_srcloc = nil,
    last_srcloc = nil,
  }, metatable)
  if token then
    self:update(token)
  end
  if attribute then
    self:update(attribute)
  end
  return self
end

---@param node dromozoa.node
---@return dromozoa.node
function class:append(node)
  table.insert(self.nodes, node)
  return self:update(node)
end

---@param nodes dromozoa.node[]
---@return dromozoa.node
function class:extend(nodes)
  for _, node in ipairs(nodes) do
    self:append(node)
  end
  return self
end

---@param that dromozoa.node|dromozoa.token
---@return dromozoa.node
function class:update(that)
  -- 空のブロックと式リストはソース位置を持たない。
  if not that.first_srcloc or not that.last_srcloc then
    assert(#that.nodes == 0)
    assert(that.category == "block" or that.category == "auxiliary" and that.kind == "expressions")
    assert(not that.first_srcloc)
    assert(not that.last_srcloc)
  else
    if not self.first_srcloc or self.first_srcloc:compare(that.first_srcloc) > 0 then
      self.first_srcloc = that.first_srcloc
    end
    if not self.last_srcloc or self.last_srcloc:compare(that.last_srcloc) < 0 then
      self.last_srcloc = that.last_srcloc
    end
  end
  return self
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
function class:require(...kinds)
  return self:require_or(kinds)
end

---@param kinds string[]
---@param message string?
---@return dromozoa.node
function class:require_or(kinds, message)
  if self:check(table.unpack(kinds)) then
    return self
  end
  if not message then
    message = "syntax error???"
  end
  -- 引数にnear用のトークンを渡すべき
  error(message .. " at " .. source_location.to_string(self.first_srcloc))
end

return class
