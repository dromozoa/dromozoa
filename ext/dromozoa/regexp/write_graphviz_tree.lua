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

local function visit(out, node, indices, index)
  index = index + 1
  indices[node] = index

  local code = node[1]
  if code ~= "[" then
    local a = node[2]
    index = visit(out, a, indices, index)
    out:write(("%d -> %d;\n"):format(indices[node], indices[a]))
    if code == "." or code == "|" then
      local b = node[3]
      index = visit(out, b, indices, index)
      out:write(("%d -> %d;\n"):format(indices[node], indices[b]))
    end
  end

  return index
end

local function write_edges(out, root, indices)
  visit(out, root, indices, 0)
end

local function visit(out, node, indices)
  local code = node[1]
  if code == "[" then
    out:write(("%d [label=\"%s\\n%d\", shape=box];\n"):format(indices[node], set_to_str(node[2]), node.timestamp))
  elseif code == "/" then
    out:write(("%d [label = \"%s %s\"];\n"):format(indices[node], code, node[3]))
    visit(out, node[2], indices)
  else
    out:write(("%d [label = \"%s\"];\n"):format(indices[node], code))
    for i = 2, #node do
      visit(out, node[i], indices)
    end
  end
end

local function write_nodes(out, root, indices)
  visit(out, root, indices)
end

local function visit(out, node, indices, index, parent_index)
  index = index + 1
  indices[node] = index

  if parent_index then
    out:write(("%d -> %d;\n"):format(parent_index, index))
  end

  local code = node[1]
  if code == "[" then
    out:write(("%d [label=\"%s\\n%d\", shape=box];\n"):format(index, set_to_str(node[2]), node.timestamp))
  else
    for i = 2, #node do
      local child_node = node[i]
      if getmetatable(child_node) == getmetatable(node) then
        index = visit(out, child_node, indices, index, indices[node])
      end
    end

    if code == "/" then
      out:write(("%d [label = \"%s %s\"];\n"):format(indices[node], code, node[3]))
    else
      out:write(("%d [label = \"%s\"];\n"):format(indices[node], code))
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

  -- local indices = {}
  -- write_edges(out, root, indices)
  -- write_nodes(out, root, indices)
  visit(out, root, {}, 0)

  out:write "}\n"

  return out
end
