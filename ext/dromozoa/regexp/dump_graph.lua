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

local function visit(out, u, color, id)
  id = id + 1
  color[u] = id

  local attrs = {}
  if u.start then
    attrs[#attrs + 1] = "fillcolor=grey"
  end
  if u.accept then
    attrs[#attrs + 1] = "shape=doublecircle"
  end
  if #attrs > 0 then
    out:write(("%d [%s];\n"):format(id, table.concat(attrs)))
  end

  local t = u.t
  for i = 1, #t do
    local e = t[i]
    local v = assert(e.v)
    local c = color[v]
    if not c then
      id = visit(out, v, color, id)
    end

    local set = e.set
    if set then
      out:write(("%d -> %d [label=\"%s\"];\n"):format(color[u], color[v], encode_set(set)))
    else
      out:write(("%d -> %d;\n"):format(color[u], color[v]))
    end
  end

  return id
end

return function (out, start)
  local color = {}

  out:write "digraph {\n"
  visit(out, start, color, 0)
  out:write "}\n"

  return out
end
