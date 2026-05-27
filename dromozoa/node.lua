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
---@field attribute dromozoa.token?
---@field nodes dromozoa.node[]
---@field first_srcloc dromozoa.source_location
---@field last_srcloc dromozoa.source_location?
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.node",
}

---@param category dromozoa.node.category
---@param kind string
---@param token dromozoa.token?
---@return dromozoa.node
function class.new(category, kind, token)
  return setmetatable({
    category = category,
    kind = kind,
    token = token,
    attribute = nil,
    nodes = {},
    first_srcloc = token and token.first_srcloc,
    last_srcloc = token and token.last_srcloc,
  }, metatable)
end

---@param node dromozoa.node
---@return dromozoa.node
function class:append(node)
  table.insert(self.nodes, node)
  return self:update_srcloc(node)
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
function class:update_first_srcloc(that)
  if not that.first_srcloc then
    -- 空のブロックもしくは式リストだけが開始位置を持たない。
    assert(#that.nodes == 0 and (that.category == "block" or that.category == "auxiliary" and that.kind == "expressions"))
  elseif not self.first_srcloc or self.first_srcloc:compare(that.first_srcloc) > 0 then
    self.first_srcloc = that.first_srcloc
  end
  return self
end

---@param that dromozoa.node|dromozoa.token
---@return dromozoa.node
function class:update_last_srcloc(that)
  if not that.last_srcloc then
    -- 空のブロックもしくは式リストだけが終了位置を持たない。
    assert(#that.nodes == 0 and (that.category == "block" or that.category == "auxiliary" and that.kind == "expressions"))
  elseif not self.last_srcloc or self.last_srcloc:compare(that.last_srcloc) < 0 then
    self.last_srcloc = that.last_srcloc
  end
  return self
end

---@param that dromozoa.node|dromozoa.token
---@return dromozoa.node
function class:update_srcloc(that)
  return self:update_first_srcloc(that):update_last_srcloc(that)
end

---@return dromozoa.source_location?
function class:srcloc()
  if self.token then
    return self.token.first_srcloc
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
