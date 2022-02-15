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

local encode_set = require "dromozoa.regexp.encode_set"

local function dump(out, node, map, id)
  id = id + 1
  map[node] = id

  local code = node[1]
  if code == "[" then
    out:write(("%d [label=\"%s\", shape=box];\n"):format(id, encode_set(node[2])))
  elseif code == "/" then
    out:write(("%d [label = \"/ %s\"];\n"):format(id, node[3]))
    id = dump(out, node[2], map, id)
    out:write(("%d -> %d;\n"):format(map[node], map[node[2]]))
  else
    out:write(("%d [label = \"%s\"];\n"):format(id, code))
    for i = 2, #node do
      local that = node[i]
      id = dump(out, that, map, id)
      out:write(("%d -> %d;\n"):format(map[node], map[that]))
    end
  end

  return id
end

return function (out, node)
  local map = {}

  out:write "digraph {\n"
  dump(out, node, map, 0)
  out:write "}\n"

  return out
end
