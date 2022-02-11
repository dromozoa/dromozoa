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

local function new_state()
  return { t = {} }
end

local function new_transition(u, v, set)
  local e = { v = v, set = set }
  local t = u.t
  t[#t + 1] = e
  return e
end

-- TODO uvをnodeのしたにはやす必要はない？

local function visit(node)
  local code = node[1]
  local a = node[2]
  local b = node[3]

  if code == "[" then
    local u = new_state()
    local v = new_state()
    local t = new_transition(u, v, a)
    node.u = u
    node.v = v
  else
    for i = 2, #node do
      visit(node[i])
    end
    if code == "." then
      new_transition(a.v, b.u)
      node.u = a.u
      node.v = b.v
    elseif code == "|" then
      local u = new_state()
      local v = new_state()
      new_transition(u, a.u)
      new_transition(u, b.u)
      new_transition(a.v, v)
      new_transition(b.v, v)
      node.u = u
      node.v = v
    elseif code == "*" then
      local u = new_state()
      local v = new_state()
      new_transition(u, a.u)
      new_transition(a.v, a.u)
      new_transition(a.v, v)
      new_transition(u, v)
      node.u = u
      node.v = v
    elseif code == "?" then
      local u = new_state()
      local v = new_state()
      new_transition(u, a.u)
      new_transition(a.v, v)
      new_transition(u, v)
      node.u = u
      node.v = v
    end
  end
end

return function (root, accept)
  visit(root)
  local u = root.u
  local v = root.v
  v.accept = accept
  return u
end
