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

---@class dromozoa.null_denotation
---@field denotation fun(self: dromozoa.parser, token: dromozoa.token): dromozoa.node
---@field prefixexp boolean
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.null_denotation",
}

---@param denotation fun(self: dromozoa.parser, token: dromozoa.token): dromozoa.node
---@param prefixexp boolean
---@return dromozoa.null_denotation
function class.new(denotation, prefixexp)
  return setmetatable({
    denotation = denotation,
    prefixexp = prefixexp,
  }, metatable)
end

return class
