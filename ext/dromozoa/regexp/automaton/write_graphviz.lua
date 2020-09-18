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

local encode_set = require "dromozoa.regexp.encode_set"

return function(transitions, start_state, action_states, final_states, out)
  local epsilons1 = transitions[256]
  local epsilons2 = transitions[257]

  out:write [[
digraph {
graph[rankdir=LR];
]]

  local u = start_state
  out:write(u, "[style=filled,fillcolor=black,fontcolor=white")
  local accept = final_states[u]
  if accept then
    out:write(",peripheries=2,label=\"", u, "/", accept, "\"")
  end
  out:write "];\n"

  for u = 1, transitions.max_state do
    if u ~= start_state then
      local accept = final_states[u]
      if accept then
        out:write(u, "[peripheries=2,label=\"", u, "/", accept, "\"];\n")
      end
    end

    if epsilons1 then
      local v = epsilons1[u]
      if v then
        out:write(u, "->", v, ";\n")
      end
    end

    if epsilons2 then
      local v = epsilons2[u]
      if v then
        out:write(u, "->", v, ";\n")
      end
    end

    local vsets = {}
    for byte = 0x00, 0xFF do
      local v = transitions[byte][u]
      if v then
        local set = vsets[v]
        if set then
          set[byte] = true
        else
          vsets[v] = { [byte] = true }
        end
      end
    end
    for v, set in pairs(vsets) do
      out:write(u, "->", v, "[label=\"", encode_set(set), "\"];\n")
    end
  end

  out:write "}\n"

  return out
end
