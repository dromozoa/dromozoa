-- Copyright (C) 2021 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local function epsilon_closure_impl(transitions, u, epsilon_closure)
  for ev = 256, #transitions do
    local v = transitions[ev][u]
    if not v then
      break
    end
    if not epsilon_closure[v] then
      epsilon_closure[v] = true
      epsilon_closure_impl(transitions, v, epsilon_closure)
    end
  end
end

local function epsilon_closure(transitions, u, epsilon_closures)
  local epsilon_closure = epsilon_closures[u]
  if epsilon_closure then
    return epsilon_closure
  else
    local epsilon_closure = { [u] = true }
    epsilon_closures[u] = epsilon_closure
    epsilon_closure_impl(transitions, u, epsilon_closure)
    return epsilon_closure
  end
end

local function set_to_seq(set)
  local seq = {}
  local n = 0
  for u in pairs(set) do
    n = n + 1
    seq[n] = u
  end
  table.sort(seq)
  return seq
end

local function insert(transitions, maps, key)
  local n = #key
  local map = maps[n]
  if not map then
    map = {}
    maps[n] = map
  end
  for i = 1, n - 1 do
    local k = key[i]
    local m = map[k]
    if not m then
      m = {}
      map[k] = m
    end
    map = m
  end
  local k = key[n]
  local v = map[k]
  if v then
    return v, false
  else
    local v = transitions:add_state()
    map[k] = v
    return v, true
  end
end

local function dump(seq)
  for i = 1, #seq do
    if i > 1 then
      io.write ", "
    end
    io.write(seq[i])
  end
  io.write "\n"
end

local function to_dfa(this, that, epsilon_closures, maps, useq, u)
  local this_transitions = this.transitions
  local this_start_state = this.start_state
  local this_action_states = this.action_states
  local this_accept_states = this.accept_states

  for ev = 0, 255 do
    local vset
    for i = 1, #useq do
      local x = this_transitions[ev][useq[i]]
      if x then
        for y in pairs(epsilon_closure(this_transitions, x, epsilon_closures)) do
          if vset then
            vset[y] = true
          else
            vset = { [y] = true }
          end
        end
      end
    end
    if vset then
      local vseq = set_to_seq(vset)
      local v, inserted = insert(transitions, maps, vseq)
      if inserted then
      end
    end
  end
end

return function (this, transitions)
  local that = {
    transitions = transitions;
    action_states = {};
    accept_states = {};
  }

  local epsilon_closures = {}
  local maps = {}

  local uset = epsilon_closure(this.transitions, this.start_state, epsilon_closures)
  local useq = set_to_seq(uset)
  local u = insert(transitions, maps, useq)

  to_dfa(this, transitions, epsilon_closures, maps, useq, u)
end
