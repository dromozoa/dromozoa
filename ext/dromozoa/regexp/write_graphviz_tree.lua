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

local set_to_str = require "dromozoa.regexp.set_to_str"

local function visit(out, v, indices, index, uid)
  index = index + 1
  indices[v] = index

  if uid then
    out:write(("%d -> %d;\n"):format(uid, index))
  end

  local code = v[1]
  if code == "[" then
    out:write(("%d [label=\"%s\\n%d\", shape=box];\n"):format(index, set_to_str(v[2]), v.timestamp))
  else
    local vid = index
    if code == "/" then
      index = visit(out, v[2], indices, index, vid)
      out:write(("%d [label = \"%s %s\"];\n"):format(vid, code, v[3]))
    else
      for i = 2, #v do
        index = visit(out, v[i], indices, index, vid)
      end
      out:write(("%d [label = \"%s\"];\n"):format(vid, code))
    end
  end

  return index
end

return function (out, root)
  out:write [[
digraph {
  graph [
    layout=dot,
  ];
]]

  -- OmniGraffleのdotレンダリングエンジンは、エッジを先に出力しないと表示順序
  -- （重ね合わせ順序）が安定しないらしい。
  visit(out, root, {}, 0)

  out:write "}\n"

  return out
end
