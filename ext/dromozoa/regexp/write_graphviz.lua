-- Copyright (C) 2020,2022 Tomoyuki Fujimori <moyu@dromozoa.com>
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
local set_to_str = require "dromozoa.regexp.set_to_str"

local function visit(out, u, indices, index, start, color)
  color[u] = 1
  index = index + 1
  indices[u] = index

  local uid = index
  local attrs = {}
  if u == start then
    -- OmniGraffleのdotレンダリングエンジンは、ラベルを定義しないとフォント色を
    -- 認識しないらしい。
    attrs[#attrs + 1] = "fillcolor=black"
    attrs[#attrs + 1] = "fontcolor=white"
  end
  local accept = u.accept
  if accept then
    attrs[#attrs + 1] = "shape=doublecircle"
  end
  if next(attrs) then
    if accept then
      attrs[#attrs + 1] = ("label=\"%d / %d\""):format(uid, accept)
    else
      attrs[#attrs + 1] = ("label=\"%d\""):format(uid)
    end
    out:write(("  %d [%s];\n"):format(uid, table.concat(attrs, ",")))
  end

  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    local v = transition.v
    if not color[v] then
      index = visit(out, v, indices, index, start, color)
    end
    local vid = indices[v]
    local set = transition.set
    if set then
      local action = transition.action
      if action then
        out:write(("  %d -> %d [label=\"%s / %d\"];\n"):format(uid, vid, set_to_str(set), action))
      else
        out:write(("  %d -> %d [label=\"%s\"];\n"):format(uid, vid, set_to_str(set)))
      end
    else
      out:write(("  %d -> %d;\n"):format(uid, vid))
    end
  end

  color[u] = 2
  return index
end

return function (out, u)
  local state_to_index, index_to_state, max_accept_index = fsm.create_state_indices(u)

  out:write [[
digraph {
  graph [layout=dot,rankdir=LR];
  node [shape=circle];
]]

  -- OmniGraffleのdotレンダリングエンジンは、表示順序（重ね合わせ順序）が安定し
  -- ないように見える。rankdir=LRの場合、安定させる条件が良くわからなかった。
  visit(out, u, {}, 0, u, {})

  out:write "}\n"
end
