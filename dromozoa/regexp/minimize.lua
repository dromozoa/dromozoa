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


--[=[
local function dummy()
  -- 逆方向の遷移表
  local reverse_transitions = {}
  build_reverse_transitions(transitions, reverse_transitions, { [start_state] = true }, start_state)

  -- 受理状態からさかのぼりつつ、IDの最大値を求める
  local color = {}
  local color_max
  for u in pairs(accept_states) do
    color[u] = true
    if not color_max or color_max < u then
      color_max = u
    end
    color_max = build_reverse_color(reverse_transitions, color, color_max, u)
  end

  -- 受理状態の集合と、非受理状態の集合に分ける
  -- ID順なのでソート済みの列
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

  -- 受理番号ごとに受理状態をパーティションにわける
  -- 最後のひとつだけが非受理状態のパーティション
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

  -- 状態IDからパーティションIDへのマップを作る
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
        -- パーティションに含まれる状態xのID
        local x = partition[i]

        -- パーティションに含まれる自分よりも前の状態yのID
        for j = 1, i - 1 do
          local y = partition[j]
          local same_partition = true
          for byte = 0, 255 do
            -- xからの遷移先の状態が含まれるパーティションと
            -- yからの遷移先の状態が含まれるパーティションが同じかどうか
            if partition_table[transitions[byte][x]] ~= partition_table[transitions[byte][y]] then
              same_partition = false
              break
            end
          end
          -- xとyの遷移先の状態が含まれるパーティションが同じならば、
          -- xとyはマージができる
          if same_partition then
            -- 新しいパーティションがなければ作成する
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

        -- xが新しいパーティションを割り当てられていなければ
        -- 孤独なパーティションを作成する
        if not new_partition_table[x] then
          local p = #new_partitions + 1
          new_partitions[p] = { x }
          new_partition_table[x] = p
        end
      end
    end
    -- partitionが変わっていなければ
    if #partitions == #new_partitions then
      break
    end
    partitions = new_partitions
    partition_table = new_partition_table
  end
end
]=]

local function visit(u, accept_partition_map, nonaccept_partition, color)
  color[u] = 1

  local accept = u.accept
  if accept then
    local partition = accept_partition_map[accept]
    if not partition then
      accept_partition_map[accept] = { u }
    else
      partition[#partition + 1] = u
    end
  else
    nonaccept_partition[#nonaccept_partition + 1] = u
  end

  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    local v = transition.v
    if not color[v] then
      visit(v, accept_partition_map, nonaccept_partition, color)
    end
  end

  color[u] = 2
end

local function create_initial_partitions(u)
  local accept_partition_map = {}
  local nonaccept_partition = {}
  visit(u, accept_partition_map, nonaccept_partition, {})

  local partitions = {}
  for _, partition in pairs(accept_partition_map) do
    partitions[#partitions + 1] = partition
  end
  table.sort(partions, function (a, b) return a[1].accept < b[1].accept end)

  partitions[#partitions + 1] = nonaccept_partition

  local partition_map = {}
  for i = 1, #partitions do
    local partition = partitions[i]
    for j = 1, #partition do
      partition_map[partition[j]] = partition
    end
  end

  return partitions, partition_map
end

local function move(u, byte)
  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    local set = transition.set
    if set and set[byte] then
      return transition.v, transition.action
    end
  end
end

return function (u)
  local partitions, partition_map = create_initial_partitions(u)

  while true do
    local new_partitions = {}
    local new_partition_map = {}

    for i = 1, #partitions do
      local partition = partitions[i]
      for j = 1, #partition do
        local x = partition[j]
        for k = 1, j - 1 do
          local y = partition[k]
          local same_partition = true
          for byte = 0x00, 0xFF do
            if partition_map[move(x, byte)] ~= partition_map[move(y, byte)] then
              same_partition = false
              break
            end
          end
          if same_partition then
            -- 順序依存はでない？
            local px = new_partition_map[x]
            local py = new_partition_map[y]
            if px then
              if not py then
                px[#px + 1] = y
                new_partition_map[y] = px
              else
                -- この場合、px == pyが保証される？
                -- そもそも、どんな場合にpxとpyが定義される？
              end
            elseif py then
              py[#py + 1] = x
              new_partition_map[x] = py
            else
              local p = { x, y }
              new_partitions[#new_partitions + 1] = p
              new_partition_map[x] = p
              new_partition_map[y] = p
            end
          end
        end
      end
    end

    -- パーティション数が増えなければ終了でよいはず？
    if #partitions == #new_partitions then
      break
    end

    paritions = new_partitions
    partition_map = new_partition_map
  end

end
