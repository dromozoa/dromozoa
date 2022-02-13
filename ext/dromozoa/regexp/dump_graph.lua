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

local graph = require "dromozoa.regexp.graph"
local encode_set = require "dromozoa.regexp.encode_set"

local function visit(out, u, state_to_index, color, start)
  color[u] = 1

  local uid = state_to_index[u]
  local attrs = {}
  if u == start then
    attrs[#attrs + 1] = "fillcolor=grey"
  end
  if u.accept then
    attrs[#attrs + 1] = "shape=doublecircle"
  end
  if #attrs > 0 then
    out:write(("%d [%s];\n"):format(uid, table.concat(attrs)))
  end

  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    local v = transition.v
    if not color[v] then
      visit(out, v, state_to_index, color, start)
    end
    local set = transition.set
    local vid = state_to_index[v]
    if set then
      out:write(("%d -> %d [label=\"%s\"];\n"):format(uid, vid, encode_set(set)))
    else
      out:write(("%d -> %d;\n"):format(uid, vid))
    end
  end
end

return function (out, start)
  local state_to_index, index_to_state, max_accept_index = graph.create_state_indices(start)
  out:write "digraph {\n"
  visit(out, start, state_to_index, {}, start)
  out:write "}\n"
  return out
end
