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

---@class dromozoa.token_reader
---@field tokens dromozoa.token[]
---@field index integer
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.token_reader",
}

---@param tokens dromozoa.token[]
---@return dromozoa.token_reader
function class.new(tokens)
  return setmetatable({
    tokens = tokens,
    index = 1,
  }, metatable)
end

---@return dromozoa.token
function class:peek()
  return self.tokens[self.index]
end

---@return dromozoa.token
function class:read()
  local token = self.tokens[self.index]
  self.index = self.index + 1
  return token
end

function class:unread()
  self.index = self.index - 1
end

return class
