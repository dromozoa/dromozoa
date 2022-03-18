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

local difference = require "dromozoa.regexp.difference"
local fsm = require "dromozoa.regexp.fsm"
local minimize = require "dromozoa.regexp.minimize"
local nfa_to_dfa = require "dromozoa.regexp.nfa_to_dfa"

local function visit(node)
  local op = node[1]
  if op == "[" then
    local u = fsm.new_state()
    local v = fsm.new_state()
    local transition = fsm.new_transition(u, v, node[2])
    transition.timestamp = node.timestamp
    return u, v
  else
    local au, av = visit(node[2])
    if op == "." then
      local bu, bv = visit(node[3])
      fsm.new_transition(av, bu)
      return au, bv
    elseif op == "|" then
      local bu, bv = visit(node[3])
      local u = fsm.new_state()
      local v = fsm.new_state()
      fsm.new_transition(u, au)
      fsm.new_transition(u, bu)
      fsm.new_transition(av, v)
      fsm.new_transition(bv, v)
      return u, v
    elseif op == "*" then
      local u = fsm.new_state()
      local v = fsm.new_state()
      fsm.new_transition(u, v)
      fsm.new_transition(u, au)
      fsm.new_transition(av, au)
      fsm.new_transition(av, v)
      return u, v
    elseif op == "+" then
      local u = fsm.new_state()
      local v = fsm.new_state()
      fsm.new_transition(u, au)
      fsm.new_transition(av, au)
      fsm.new_transition(av, v)
      return u, v
    elseif op == "?" then
      local u = fsm.new_state()
      local v = fsm.new_state()
      fsm.new_transition(u, v)
      fsm.new_transition(u, au)
      fsm.new_transition(av, v)
      return u, v
    elseif op == "-" then
      local bu, bv = visit(node[3])
      local u = fsm.new_state()
      local v = fsm.new_state()

      -- 計算のためにaccept_actionとtimestampを割り当てる
      local timestamp = node.timestamp
      au.timestamp = timestamp
      av.accept_action = true
      av.timestamp = timestamp
      bu.timestamp = timestamp
      bv.accept_action = true
      bv.timestamp = timestamp

      local cu, accept_states = minimize(difference(minimize(nfa_to_dfa(au)), minimize(nfa_to_dfa(bu))))

      -- 計算結果からaccept_actionとtimestampを除去する
      cu.timestamp = nil
      fsm.new_transition(u, cu)
      for i = 1, #accept_states do
        local cv = accept_states[i]
        cv.accept_action = nil
        cv.timestamp = nil
        fsm.new_transition(cv, v)
      end

      return u, v
    elseif op == "/" then
      local transition = au.transitions[1]
      transition.action = node[3]
      return au, av
    elseif op == "%" then
      av.accept_action = node[3]
      av.timestamp = node.timestamp
      return au, av
    else
      error "not supported"
    end
  end
end

return function (root, accept_action)
  local u, v = visit(root)
  local timestamp = root.timestamp
  u.timestamp = timestamp
  if not v.accept_action then
    v.accept_action = accept_action or true
    v.timestamp = timestamp
  end
  return u, v
end
