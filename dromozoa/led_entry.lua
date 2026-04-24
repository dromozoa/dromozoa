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

---@class dromozoa.led_entry
---@field bp integer
---@field denotion fun(self: dromozoa.lua_parser, token: dromozoa.token, left: dromozoa.node, min_bp: integer): dromozoa.node
---@field prefixexp boolean
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.led_entry",
}

---@param bp integer
---@param denotion fun(self: dromozoa.lua_parser, token: dromozoa.token, left: dromozoa.node, min_bp: integer): dromozoa.node
---@param prefixexp boolean
---@return dromozoa.led_entry
function class.new(bp, denotion, prefixexp)
  return setmetatable({
    bp = bp,
    denotion = denotion,
    prefixexp = prefixexp,
  }, metatable)
end

return class
