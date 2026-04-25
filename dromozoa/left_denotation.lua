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

---@class dromozoa.left_denotation
---@field lbp integer
---@field fn fun(self: dromozoa.parser, left: dromozoa.node, token: dromozoa.token, rbp: integer): dromozoa.node
---@field prefixexp boolean
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.left_denotation",
}

---@param lbp integer
---@param fn fun(self: dromozoa.parser, left: dromozoa.node, token: dromozoa.token, rbp: integer): dromozoa.node
---@param prefixexp boolean
---@return dromozoa.left_denotation
function class.new(lbp, fn, prefixexp)
  return setmetatable({
    lbp = lbp,
    fn = fn,
    prefixexp = prefixexp,
  }, metatable)
end

---@param parser dromozoa.parser
---@param left dromozoa.node
---@param token dromozoa.token
---@return dromozoa.node
function class:led(parser, left, token)
  return self.fn(parser, left, token, self.lbp)
end

return class
