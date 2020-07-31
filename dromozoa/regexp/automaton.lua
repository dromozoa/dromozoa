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
  that.accept_states[u] = merge_accept_state(self.accept_states, uset)

  return that
end

local function build_reverse_transitions(transitions, reverse_transitions, color, u)
  for byte = 0x00, 0xFF do
    local v = transitions[byte][u]
    if v and u ~= v then
      local reverse_transition = reverse_transitions[v]
      if reverse_transition then
        reverse_transition[u] = true
      else
        reverse_transitions[v] = { [u] = true }
      end
      if not color[v] then
        color[v] = true
        build_reverse_transitions(transitions, reverse_transitions, color, v)
      end
    end
  end
end

local function build_reverse_color(reverse_transitions, color, color_max, u)
  local reverse_transition = reverse_transitions[u]
  if reverse_transition then
    for v in pairs(reverse_transition) do
      if not color[v] then
        color[v] = true
        if color_max < v then
          color_max = v
        end
        color_max = build_reverse_color(reverse_transitions, color, color_max, v)
      end
    end
  end
  return color_max
end

function class:minimize()
  local transitions = self.transitions
  local start_state = self.start_state
  local accept_states = self.accept_states

  local reverse_transitions = {}
  build_reverse_transitions(transitions, reverse_transitions, { [start_state] = true }, start_state)

  local color = {}
  local color_max
  for u in pairs(accept_states) do
    color[u] = true
    if not color_max or color_max < u then
      color_max = u
    end
    color_max = build_reverse_color(reverse_transitions, color, color_max, u)
  end

  local accept_partitions = {}
  local nonaccept_partition
  for u = 1, color_max do
    if color[u] then
      local accept = accept_states[u]
      if accept then
        local partition = accept_partitions[accept]
        if partition then
          partition[#partition + 1] = u
        else
          accept_partitions[accept] = { u }
        end
      else
        if nonaccept_partition then
          nonaccept_partition[#nonaccept_partition + 1] = u
        else
          nonaccept_partition = { u }
        end
      end
    end
  end

  local partitions = {}
  local partition_table = {}
  local n = 0
  for _, partition in pairs(accept_partitions) do
    n = n + 1
    partitions[n] = partition
  end
  if nonaccept_partition then
    n = n + 1
    partitions[n] = nonaccept_partition
  end

  local partition_table = {}
  for i = 1, #partitions do
    local partition = partitions[i]
    for j = 1, #partition do
      partition_table[partition[j]] = i
    end
  end

  while true do
    local new_partitions = {}
    local new_partition_table = {}
    for i = 1, #partitions do
      local partition = partitions[i]
      for i = 1, #partition do
        local x = partition[i]
        for j = 1, i - 1 do
          local y = partition[j]
          local same_partition = true
          for byte = 0, 255 do
            if partition_table[transitions[byte][x]] ~= partition_table[transitions[byte][y]] then
              same_partition = false
              break
            end
          end
          if same_partition then
            local px = new_partition_table[x]
            local py = new_partition_table[y]
            if px then
              if not py then
                local new_partition = new_partitions[px]
                new_partition[#new_partition + 1] = y
                new_partition_table[y] = px
              end
            elseif py then
              local new_partition = new_partitions[py]
              new_partition[#new_partition + 1] = x
              new_partition_table[x] = py
            else
              local p = #new_partitions + 1
              new_partitions[p] = { x, y }
              new_partition_table[x] = p
              new_partition_table[y] = p
            end
          end
        end
        if not new_partition_table[x] then
          local p = #new_partitions + 1
          new_partitions[p] = { x }
          new_partition_table[x] = p
        end
      end
    end
    if #partitions == #new_partitions then
      break
    end
    partitions = new_partitions
    partition_table = new_partition_table
  end

  local max_state = #partitions
  local new_transitions = {}
  for byte = 0, 255 do
    new_transitions[byte] = {}
  end
  local new_accept_states = {}

  for i = 1, max_state do
    local partition = partitions[i]
    local u = partition[1]
    for byte = 0, 255 do
      local v = transitions[byte][u]
      if v then
        new_transitions[byte][i] = partition_table[v]
      end
    end
    new_accept_states[i] = accept_states[u]
  end

  local that = new()
  that.max_state = max_state
  that.transitions = new_transitions
  that.start_state = partition_table[start_state]
  that.accept_states = new_accept_states
  return that
end

return class
