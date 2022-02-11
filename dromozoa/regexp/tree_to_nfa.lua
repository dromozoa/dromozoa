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

local function visit(node)
  local code = node[1]
  if code == "[" then
    local u = new_state()
    local v = new_state()
    local t = new_transition(u, v, node[2])
    return u, v
  else
    local au, av = visit(node[2])
    if code == "." then
      local bu, bv = visit(node[3])
      new_transition(av, bu)
      return au, bv
    elseif code == "|" then
      local bu, bv = visit(node[3])
      local u = new_state()
      local v = new_state()
      new_transition(u, au)
      new_transition(u, bu)
      new_transition(av, v)
      new_transition(bv, v)
      return u, v
    elseif code == "*" then
      local u = new_state()
      local v = new_state()
      new_transition(u, au)
      new_transition(av, au)
      new_transition(av, v)
      new_transition(u, v)
      return u, v
    elseif code == "?" then
      local u = new_state()
      local v = new_state()
      new_transition(u, au)
      new_transition(av, v)
      new_transition(u, v)
      return u, v
    end
  end
end

return function (root, accept)
  local u, v = visit(root)
  v.accept = accept
  return u
end
