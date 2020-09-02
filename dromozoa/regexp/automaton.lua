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

-- (transitions, start_state, accept_states)
function class.new()
  local transitions = {}
  for i = 0, 257 do
    transitions[i] = {}
  end
  -- TODO start_state, final_stateをオブジェクトの外においだす
  return setmetatable({
    max_state = 0;
    transitions = transitions;
    start_state = nil;
    accept_states = {};
  }, metatable)
end

function class:new_state()
  local max_state = self.max_state + 1
  self.max_state = max_state
  return max_state
end

function class:new_transition(u, v, set)
  if set then
    local transitions = self.transitions
    for i in pairs(set) do
      transitions[i][u] = v
    end
  else
    local transitions = self.transitions
    for i = 256, #transitions do
      local epsilons = transitions[i]
      if not epsilons[u] then
        epsilons[u] = v
        return
      end
    end
    -- TODO raise error
  end
end

do
  local function epsilon_closure_impl(t, epsilon_closure, u)
    for i = 256, #t do
      local v = t[i][u]
      if v and not epsilon_closure[v] then
        epsilon_closure[v] = true
        epsilon_closure_impl(t, epsilon_closure, v)
      end
    end
  end

  local function epsilon_closure(t, epsilon_closures, u)
    local epsilon_closure = epsilon_closures[u]
    if epsilon_closure then
      return epsilon_closure
    else
      local epsilon_closure = { [u] = true }
      epsilon_closures[u] = epsilon_closure
      epsilon_closure_impl(t, epsilon_closure, u)
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

  local function insert(self, maps, key)
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
      local v = self:new_state()
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

    for i = 0, 255 do
      local vset
      for j = 1, #useq do
        local x = transitions[i][useq[j]]
        if x then
          for y in pairs(epsilon_closure(transitions, epsilon_closures, x)) do
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
        new_transitions[i][u] = v
        if inserted then
          new_accept_states[v] = merge_accept_state(accept_states, vset)
          to_dfa(self, that, epsilon_closures, maps, vseq, v)
        end
      end
    end
  end

  -- new_start_state, new_accept_states
  function class:to_dfa()
    local epsilon_closures = {}
    local maps = {}
    local uset = epsilon_closure(self.transitions, epsilon_closures, self.start_state)
    local useq = set_to_seq(uset)
    local that = class.new()
    local u = insert(that, maps, useq)

    to_dfa(self, that, epsilon_closures, maps, useq, u)

    that.start_state = u
    that.accept_states[u] = merge_accept_state(self.accept_states, uset)

    return that
  end
end

do
  local function epsilon_closure_impl(t, epsilon_closure, u)
    for byte = 256, #t do
      local v = t[byte][u]
      if v then
        if not epsilon_closure[v] then
          epsilon_closure[v] = true
          epsilon_closure_impl(t, epsilon_closure, v)
        end
      end
    end
  end

  local function epsilon_closure(t, epsilon_closures, u)
    local epsilon_closure = epsilon_closures[u]
    if epsilon_closure then
      return epsilon_closure
    else
      local epsilon_closure = { [u] = true }
      epsilon_closures[u] = epsilon_closure
      epsilon_closure_impl(t, epsilon_closure, u)
      return epsilon_closure
    end
  end

  -- t: transitions
  -- u: start state
  -- v: final state
  function class:dfa__(t, u, v)
    local uset = epsilon_closure(t, epsilon_closures, u)

  end
end

do
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
            for byte = 0x00, 0xFF do
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
    local that = class.new()
    local new_transitions = that.transitions
    local new_accept_states = that.accept_states

    for i = 1, max_state do
      local partition = partitions[i]
      local u = partition[1]
      for byte = 0x00, 0xFF do
        local v = transitions[byte][u]
        if v then
          new_transitions[byte][i] = partition_table[v]
        end
      end
      new_accept_states[i] = accept_states[u]
    end

    that.max_state = max_state
    that.start_state = partition_table[start_state]
    return that
  end
end

function class.difference(this, that)
  local this_max_state = this.max_state
  local this_transitions = this.transitions
  local that_max_state = that.max_state
  local that_transitions = that.transitions
  local that_accept_states = that.accept_states

  local n = this_max_state + 1

  local result = class.new()
  local result_transitions = result.transitions
  local result_accept_states = result.accept_states

  for i = 0, this_max_state do
    for j = 0, that_max_state do
      local u = i + n * j
      if u ~= 0 then
        for byte = 0x00, 0xFF do
          local x = this_transitions[byte][i]
          local y = that_transitions[byte][j]
          if not x then
            x = 0
          end
          if not y then
            y = 0
          end
          local v = x + n * y
          if v ~= 0 then
            result_transitions[byte][u] = v
          end
        end
      end
    end
  end

  for i, accept in pairs(this.accept_states) do
    result_accept_states[i] = accept
    for j = 1, that_max_state do
      if not that_accept_states[j] then
        local u = i + n * j
        result_accept_states[u] = accept
      end
    end
  end

  result.max_state = this_max_state + n * that_max_state
  result.start_state = this.start_state + n * this.start_state

  return result
end

do
  local function remove_unreachable_states(this, result, reachable_states, u)
    local transitions = this.transitions
    local result_transitions = result.transitions

    local U = result:new_state()
    reachable_states[u] = U
    for byte = 0x00, 0xFF do
      local v = transitions[byte][u]
      if v then
        local V = reachable_states[v]
        if not V then
          V = remove_unreachable_states(this, result, reachable_states, v)
        end
        result_transitions[byte][U] = V
      end
    end
    result.accept_states[U] = this.accept_states[u]

    return U
  end

  function class.remove_unreachable_states(a, b)
    a.start_state = remove_unreachable_states(b, a, {}, b.start_state)
    return a
  end

  -- function class.remove_unreachable_states(this)
  --   local result = new()
  --   result.start_state = remove_unreachable_states(this, result, {}, this.start_state)
  --   return result
  -- end
end

do
end

return class
