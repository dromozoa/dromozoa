-- Copyright (C) 2020-2022 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local function visit(node)
  local code = node[1]
  if code == "[" then
    local u = fsm.new_state()
    local v = fsm.new_state()
    local transition = fsm.new_transition(u, v, node[2])
    transition.timestamp = node.timestamp
    return u, v
  else
    local au, av = visit(node[2])
    if code == "." then
      local bu, bv = visit(node[3])
      fsm.new_transition(av, bu)
      return au, bv
    elseif code == "|" then
      local bu, bv = visit(node[3])
      local u = fsm.new_state()
      local v = fsm.new_state()
      fsm.new_transition(u, au)
      fsm.new_transition(u, bu)
      fsm.new_transition(av, v)
      fsm.new_transition(bv, v)
      return u, v
    elseif code == "*" then
      -- TODO 状態数を減らしたNFAの構築を検討する
      local u = fsm.new_state()
      local v = fsm.new_state()
      fsm.new_transition(u, v)
      fsm.new_transition(u, au)
      fsm.new_transition(av, au)
      fsm.new_transition(av, v)
      return u, v
    elseif code == "?" then
      local u = fsm.new_state()
      local v = fsm.new_state()
      fsm.new_transition(u, v)
      fsm.new_transition(u, au)
      fsm.new_transition(av, v)
      return u, v
    elseif code == "/" then
      local transition = au.transitions[1]
      transition.action = node[3]
      return au, av
    else
      error "not supported"
    end
  end
end

return function (root, accept)
  if root[1] == "%" then
    local u, v = visit(root[2])
    v.accept = root[3]
    v.timestamp = root.timestamp
    return u, v
  else
    local u, v = visit(root)
    v.accept = accept or true
    v.timestamp = root.timestamp
    return u, v
  end
end
