-- Copyright (C) 2022 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local fsm = require "dromozoa.regexp.fsm"
local minimize = require "dromozoa.regexp.minimize"
local nfa_to_dfa = require "dromozoa.regexp.nfa_to_dfa"
local tree_to_nfa = require "dromozoa.regexp.tree_to_nfa"

return function (that)
  local n = #that
  if n == 1 then
    return minimize(nfa_to_dfa(tree_to_nfa(that[1])))
  else
    local u = fsm.new_state()
    local timestamp
    for i = 1, n do
      local v = tree_to_nfa(that[i])
      fsm.new_transition(u, v)
      timestamp = fsm.merge_timestamp(timestamp, v.timestamp)
    end
    u.timestamp = timestamp
    return minimize(nfa_to_dfa(u))
  end
end
