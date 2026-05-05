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

local class = {}
local metatable = { __index = class }

function class.new(message)
  local self = setmetatable({ message = message }, metatable)
  print("new " .. self:to_string())
  return self
end

function class:to_string()
  return tostring(self) .. " (" .. self.message .. ")"
end

function metatable:__close()
  print("close " .. self:to_string())
end

do
  local x <const> = class.new "local x <const>"
end

do
  local x <close> = class.new "local x <close>"
end

do
  local <close> x = class.new "local <close> x"
end

do
  local <const> x <close>, y = class.new "local <const> x <close>", class.new "local <const> y"
end

do
  local <close> x <const>, y = class.new "local <close> x <const>", class.new "local <close> y"
end

do
  -- multiple to-be-closed variables in local list
  -- local <close> x, y = class.new "local <close> x <close>", class.new "local <const> y"
end

do
  global X <const> = class.new "global X <const>"
end

do
  -- global variables cannot be to-be-closed
  -- global X <close> = class.new "global X <close>"
end
