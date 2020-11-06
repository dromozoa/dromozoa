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

return function (this, out)
  local transitions = this.transitions
  local start_state = this.start_state
  local action_states = this.action_states
  local accept_states = this.accept_states

  out:write [[
digraph {
graph[rankdir=LR];
]]

  for u = 1, transitions.max_state do
    local attr = {}
    local name = { u }
    local action = action_states[u]
    local accept = accept_states[u]

    if u == start_state then
      attr[#attr + 1] = "style=filled,fillcolor=black,fontcolor=white"
    end
    if action then
      name[#name + 1] = "@" .. action
    end
    if accept then
      attr[#attr + 1] = "peripheries=2"
      name[#name + 1] = "/" .. accept
    end

    attr[#attr + 1] = "label=\"" .. table.concat(name) .. "\""
    out:write(u, "[", table.concat(attr, ","), "];\n")

    local vsets = {}
    for ev = 0, 255 do
      local v = transitions[ev][u]
      if v then
        local set = vsets[v]
        if set then
          set[ev] = true
        else
          vsets[v] = { [ev] = true }
        end
      end
    end
    for v, set in pairs(vsets) do
      out:write(u, "->", v, "[label=\"", encode_set(set), "\"];\n")
    end

    for ev = 256, #transitions do
      local v = transitions[ev][u]
      if v then
        out:write(u, "->", v, ";\n")
      else
        break
      end
    end
  end

  out:write "}\n"

  return out
end
