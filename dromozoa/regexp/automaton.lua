-- Copyright (C) 2020 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local class = {}
local metatable = { __index = class }

local function new()
  local transitions = {}
  for byte = 0x00, 0xFF do
    transitions[byte] = {}
  end
  return setmetatable({
    max_state = 0;
    transitions = transitions;
    start_state = nil;
    accept_states = {};
  }, metatable)
end

function class.nfa()
  local self = new()
  self.epsilons1 = {}
  self.epsilons2 = {}
  return self
end

function class:new_state()
  local max_state = self.max_state + 1
  self.max_state = max_state
  return max_state
end

function class:new_transition(u, v, set)
  if set then
    local transitions = self.transitions
    for byte in pairs(set) do
      transitions[byte][u] = v
    end
  else
    local epsilons = self.epsilons1
    if epsilons[u] then
      epsilons = self.epsilons2
    end
    epsilons[u] = v
  end
end

local function epsilon_closure_impl(epsilons1, epsilons2, epsilon_closure, u)
  local v = epsilons1[u]
  if v then
    if not epsilon_closure[v] then
      epsilon_closure[v] = true
      epsilon_closure_impl(epsilons1, epsilons2, epsilon_closure, v)
    end
    local v = epsilons2[u]
    if v and not epsilon_closure[v] then
      epsilon_closure[v] = true
      epsilon_closure_impl(epsilons1, epsilons2, epsilon_closure, v)
    end
  end
end

local function epsilon_closure(self, epsilon_closures, u)
  local epsilon_closure = epsilon_closures[u]
  if epsilon_closure then
    return epsilon_closure
  else
    local epsilon_closure = { [u] = true }
    epsilon_closures[u] = epsilon_closure
    epsilon_closure_impl(self.epsilons1, self.epsilons2, epsilon_closure, u)
    return epsilon_closure
  end
end

local function set_to_seq(set)
  local seq = {}
  local n = 0
  for u in pairs(set) do
    n = n + 1; seq[n] = u
  end
  table.sort(seq)
  return seq
end

--[[
  key = { a, b, c, d }
  maps[4][a][b][c][d] = value
  TODO: 文字列連結より効率的であることを確認する
]]
local function insert(that, maps, key)
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
    local v = that:new_state()
    map[k] = v
    return v, true
  end
end

local function merge_accept_state(accept_states, set)
  local result
  for u in pairs(set) do
    local accept = accept_states[u]
    if accept and (not result or result > accept) then
      result = accept
    end
  end
  return result
end

local function to_dfa(self, that, epsilon_closures, maps, useq, u)
  local transitions = self.transitions
  local accept_states = self.accept_states
  local new_transitions = that.transitions
  local new_accept_states = that.accept_states

  for byte = 0x00, 0xFF do
    local vset
    for i = 1, #useq do
      local x = transitions[byte][useq[i]]
      if x then
        for y in pairs(epsilon_closure(self, epsilon_closures, x)) do
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
      local v, inserted = insert(that, maps, vseq)
      new_transitions[byte][u] = v
      if inserted then
        new_accept_states[v] = merge_accept_state(accept_states, vset)
        to_dfa(self, that, epsilon_closures, maps, vseq, v)
      end
    end
  end
end

function class:to_dfa()
  local epsilon_closures = {}
  local maps = {}
  local uset = epsilon_closure(self, epsilon_closures, self.start_state)
  local useq = set_to_seq(uset)
  local that = new()
  local u = insert(that, maps, useq)

  to_dfa(self, that, epsilon_closures, maps, useq, u)

  that.start_state = u
  -- that.accept_states[?] = ?
  that.accept_states[u] = merge_accept_state(self.accept_states, uset)

  return that
end

return class
