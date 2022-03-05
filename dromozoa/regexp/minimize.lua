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

local fsm = require "dromozoa.regexp.fsm"

local function visit(u, accept_partition_map, nonaccept_partition, partition_map, color)
  color[u] = 1

  local accept = u.accept
  local partition = nonaccept_partition
  if accept then
    partition = accept_partition_map[accept]
    if not partition then
      partition = {}
      accept_partition_map[accept] = partition
    end
  end
  partition[#partition + 1] = u
  partition_map[u] = partition

  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    local v = transition.v
    if not color[v] then
      visit(v, accept_partition_map, nonaccept_partition, partition_map, color)
    end
  end

  color[u] = 2
end

local function create_initial_partitions(u)
  local accept_partition_map = {}
  local nonaccept_partition = {}
  local partition_map = {}
  visit(u, accept_partition_map, nonaccept_partition, partition_map, {})

  local partitions = {}
  for _, partition in pairs(accept_partition_map) do
    partitions[#partitions + 1] = partition
  end
  table.sort(partitions, function (a, b) return a[1].accept < b[1].accept end)

  partitions[#partitions + 1] = nonaccept_partition

  return partitions, partition_map
end

local function execute_transition(u, byte)
  local transition = fsm.execute_transition(u, byte)
  if transition then
    return transition.v, transition.action
  end
end

return function (u)
  local partitions, partition_map = create_initial_partitions(u)

  while true do
    local new_partitions = {}
    local new_partition_map = {}

    for i = 1, #partitions do
      local partition = partitions[i]
      -- あるパーティションに含まれる状態の組 (x, y) が同じ遷移をするならば、ひ
      -- とつのパーティションにまとめる。
      for j = 1, #partition do
        local x = partition[j]
        for k = 1, j - 1 do
          local y = partition[k]

          -- 全ての文字について下記の条件が満たされたら、同じ遷移をするとみなす。
          -- 1. 遷移先の状態が同じパーティションに含まれている
          -- 2. 同じ遷移アクションを持つ
          local same_partition = true
          for byte = 0x00, 0xFF do
            local xv, xaction = execute_transition(x, byte)
            local yv, yaction = execute_transition(y, byte)
            if partition_map[xv] ~= partition_map[yv] or xaction ~= yaction then
              same_partition = false
              break
            end
          end

          if same_partition then
            local p = new_partition_map[x]
            local q = assert(new_partition_map[y])
            assert(not p or p == q)
            q[#q + 1] = x
            new_partition_map[x] = q
          end
        end

        if not new_partition_map[x] then
          local p = { x }
          new_partitions[#new_partitions + 1] = p
          new_partition_map[x] = p
        end
      end
    end

    if #partitions == #new_partitions then
      break
    end

    partitions = new_partitions
    partition_map = new_partition_map
  end

  local states = {}
  for i = 1, #partitions do
    -- あるパーティションに含まれる全ての状態のacceptは同一
    local partition = partitions[i]
    local unew = fsm.new_state()
    unew.accept = partition[1].accept
    states[partition] = { index = i, state = unew }
  end

  for i = 1, #partitions do
    local partition = partitions[i]
    local unew = states[partition].state

    -- 全ての遷移を調べてtimestampを決定する
    local new_transition_map = {}
    for byte = 0x00, 0xFF do
      local timestamp
      local action
      local vnew
      local vnew_index
      for i = 1, #partition do
        local transition = fsm.execute_transition(partition[i], byte)
        if transition then
          local t = transition.timestamp
          if not timestamp or timestamp > t then
            timestamp = t
            action = transition.action
            vnew = states[partition_map[transition.v]].state
            vnew_index = states[partition_map[transition.v]].index
          end
        end
      end

      if vnew_index then
        local new_transition_key = vnew_index .. ";" .. timestamp
        local new_transition = new_transition_map[new_transition_key]
        if not new_transition then
          new_transition = fsm.new_transition(unew, vnew, { [byte] = true })
          new_transition.action = action
          new_transition.timestamp = timestamp
          new_transition_map[new_transition_key] = new_transition
        else
          new_transition.set[byte] = true
        end
      end
    end
  end

  return states[partition_map[u]].state
end
