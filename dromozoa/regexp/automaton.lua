-- Copyright (C) 2020 Tomoyuki Fujimori <moyu@dromozoa.com>
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
-- along with dromozoa.  If not, see <http://www.gnu.org/licenses/>.

local class = {}
local metatable = { __index = class }

function class.nfa()
  local transitions = {}
  for byte = 0x00, 0xFF do
    transitions[byte] = {}
  end
  return setmetatable({
    max_state = 0;
    epsilons1 = {};
    epsilons2 = {};
    transitions = transitions;
    start_state = nil;
    accept_states = {};
  }, metatable)
end

function class:new_state()
  local max_state = self.max_state + 1
  self.max_state = max_state
  return max_state
end

function class:new_transition(u, v, set)
  if set then
    local transitions = self.transitions
    for byte in pairs(set) do
      transitions[byte][u] = v
    end
  else
    local epsilons = self.epsilons1
    if epsilons[u] then
      epsilons = self.epsilons2
    end
    epsilons[u] = v
  end
end

return class
