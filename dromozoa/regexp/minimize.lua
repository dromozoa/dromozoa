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

local assertion = false

local function visit(u, accept_partition_map, nonaccept_partition, partition_map, color)
  color[u] = 1

  local accept_action = u.accept_action
  local partition = nonaccept_partition
  if accept_action then
    partition = accept_partition_map[accept_action]
    if not partition then
      partition = { timestamp = u.timestamp }
      accept_partition_map[accept_action] = partition
    else
      local timestamp = u.timestamp
      if partition.timestamp > timestamp then
        partition.timestamp = timestamp
      end
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
  table.sort(partitions, function (a, b)
    return a.timestamp < b.timestamp
  end)

  if #nonaccept_partition > 0 then
    partitions[#partitions + 1] = nonaccept_partition
  end

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
            if partition_map[xv] ~= partition_map[yv] or not rawequal(xaction, yaction) then
              same_partition = false
              break
            end
          end

          if same_partition then
            local new_partition = new_partition_map[x]
            if not new_partition then
              local new_partition = new_partition_map[y]
              new_partition[#new_partition + 1] = x
              new_partition_map[x] = new_partition
            else
              -- 新パーティションに登録済みである
              if assertion then
                assert(new_partition == new_partition_map[y])
              end
            end
          end
        end

        if not new_partition_map[x] then
          local new_partition = { x }
          new_partitions[#new_partitions + 1] = new_partition
          new_partition_map[x] = new_partition
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
  local accept_states = {}
  for i = 1, #partitions do
    local partition = partitions[i]

    local accept_action = partition[1].accept_action
    local timestamp
    if accept_action then
      timestamp = partition[1].timestamp
      for j = 2, #partition do
        local x = partition[j]
        local t = x.timestamp
        if timestamp > t then
          timestamp = t
        end
      end
    end

    local unew = fsm.new_state()
    unew.accept_action = accept_action
    unew.timestamp = timestamp
    states[partition] = { key = i, state = unew }
    if accept_action then
      accept_states[#accept_states + 1] = unew
    end
  end

  for i = 1, #partitions do
    local partition = partitions[i]
    local unew = states[partition].state

    local action_index = 0
    local actions = {}
    local new_transition_map = {}

    for byte = 0x00, 0xFF do
      local transition = fsm.execute_transition(partition[1], byte)

      if transition then
        local timestamp = transition.timestamp
        local action = transition.action
        local v = states[partition_map[transition.v]]
        local vkey = v.key
        local vnew = v.state

        -- パーティションに含まれる各状態は同じ遷移をする
        for j = 2, #partition do
          local transition = fsm.execute_transition(partition[j], byte)
          if assertion then
            assert(rawequal(action, transition.action))
            assert(vkey == states[partition_map[transition.v]].key)
            assert(vnew == states[partition_map[transition.v]].state)
          end
          local t = transition.timestamp
          if not timestamp or timestamp > t then
            timestamp = t
          end
        end

        local new_transition_key
        if action then
          -- actionの生の比較 (raw equality) を行うためにテーブルを経由する
          local index = actions[action]
          if not index then
            action_index = action_index + 1
            actions[action] = action_index
            new_transition_key = vkey .. ";" .. action_index
          else
            new_transition_key = vkey .. ";" .. index
          end
        else
          new_transition_key = tostring(vkey)
        end

        local new_transition = new_transition_map[new_transition_key]
        if not new_transition then
          new_transition = fsm.new_transition(unew, vnew, { [byte] = true })
          new_transition.action = action
          new_transition.timestamp = timestamp
          new_transition_map[new_transition_key] = new_transition
        else
          if assertion then
            assert(rawequal(action, new_transition.action))
            assert(vnew == new_transition.v)
          end
          new_transition.set[byte] = true
          if new_transition.timestamp > timestamp then
            new_transition.timestamp = timestamp
          end
        end
      else
        if assertion then
          for j = 2, #partition do
            assert(not fsm.execute_transition(partition[j], byte))
          end
        end
      end
    end
  end

  local unew = states[partition_map[u]].state
  unew.timestamp = u.timestamp
  return unew, accept_states
end
