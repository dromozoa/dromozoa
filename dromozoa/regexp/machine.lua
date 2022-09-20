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

local append = require "dromozoa.append"
local compare = require "dromozoa.compare"
local tree_map = require "dromozoa.tree_map"

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.regexp.machine.state" }

function class:simulate(byte, resolved)
  for _, t in ipairs(self.transitions) do
    if t.set and t.set[byte] then
      if resolved and (not resolved.timestamp or resolved.timestamp > t.timestamp) then
        resolved.timestamp = t.timestamp
        resolved.action = t.action
      end
      return t.v, t.timestamp, t.action
    end
  end
end

function class:update(timestamp, accept_action)
  if timestamp and accept_action and (not self.timestamp or self.timestamp > timestamp) then
    self.timestamp = timestamp
    self.accept_action = accept_action
  end
  return self
end

local function state()
  return setmetatable({ transitions = {} }, metatable)
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.regexp.machine.transition" }

function class:update(timestamp, byte)
  if self.timestamp > timestamp then
    self.timestamp = timestamp
  end
  self.set[byte] = true
  return self
end

local function transition(u, v, set, timestamp, action)
  local self = setmetatable({ v = v, set = set, timestamp = timestamp, action = action }, metatable)
  append(u.transitions, self)
  return self
end

---------------------------------------------------------------------------

local difference

local function node_to_nfa(node)
  local code = node[0]
  if code == "[" then
    local u = state()
    local v = state()
    transition(u, v, node[1], node.timestamp)
    return u, v
  else
    local au, av = node_to_nfa(node[1])
    if code == "/" then
      assert(#au.transitions == 1)
      au.transitions[1].action = node[2]
      return au, av
    elseif code == "%" then
      av:update(node.timestamp, node[2])
      return au, av
    elseif code == "." then
      local bu, bv = node_to_nfa(node[2])
      transition(av, bu)
      return au, bv
    else
      local u = state()
      local v = state()
      if code == "*" then
        transition(u, v)
        transition(u, au)
        transition(av, au)
        transition(av, v)
      elseif code == "+" then
        transition(u, au)
        transition(av, au)
        transition(av, v)
      elseif code == "?" then
        transition(u, v)
        transition(u, au)
        transition(av, v)
      else
        local bu, bv = node_to_nfa(node[2])
        if code == "|" then
          transition(u, au)
          transition(u, bu)
          transition(av, v)
          transition(bv, v)
        elseif code == "-" then
          av:update(node.timestamp, "")
          bv:update(node.timestamp, "")
          local cu, accept_states = difference(au, bu)
          transition(u, cu)
          for _, cv in ipairs(accept_states) do
            cv.timestamp = nil
            cv.accept_action = nil
            transition(cv, v)
          end
        end
      end
      return u, v
    end
  end
end

local function tree_to_nfa(node)
  local u, v = node_to_nfa(node)
  if not v.accept_action then
    v:update(node.timestamp, "")
  end
  return u, v
end

---------------------------------------------------------------------------

local function update_state_indices_impl(u, states, color)
  color[u] = 1
  u.index = append(states, u)
  for _, t in ipairs(u.transitions) do
    if not color[t.v] then
      update_state_indices_impl(t.v, states, color)
    end
  end
  color[u] = 2
end

local function update_state_indices(u)
  local states = {}
  update_state_indices_impl(u, states, {})
  return states
end

---------------------------------------------------------------------------

local function epsilon_closure_key(closure)
  local result = {}
  for k in pairs(closure.map) do
    append(result, k)
  end
  if result[1] then
    table.sort(result)
    local key = table.concat(result, ",")
    closure.key = key
    return key
  end
end

local function epsilon_closure_impl(u, closure)
  for _, t in ipairs(u.transitions) do
    if not t.set then
      local v = t.v
      closure.map[v.index] = v
      epsilon_closure_impl(v, closure)
    end
  end
end

local function epsilon_closure(u, epsilon_closures)
  local closure = epsilon_closures[u]
  if not closure then
    closure = { map = { [u.index] = u } }
    epsilon_closure_impl(u, closure)
    epsilon_closures[u] = closure
    epsilon_closure_key(closure)
  end
  return closure
end

local function closure_to_state(closure, states)
  local u = states[closure.key]
  if not u then
    u = state()
    for _, v in pairs(closure.map) do
      u:update(v.timestamp, v.accept_action)
    end
    states[closure.key] = u
  end
  return u
end

local function nfa_to_dfa_impl(u_closure, u, epsilon_closures, states, color)
  local ukey = u_closure.key
  color[ukey] = 1

  local vmap = {}
  local tmap = {}

  for byte = 0x00, 0xFF do
    local resolved = {}
    local v_closure = { map = {} }
    for _, u in pairs(u_closure.map) do
      local to = u:simulate(byte, resolved)
      if to then
        for _, v in pairs(epsilon_closure(to, epsilon_closures).map) do
          v_closure.map[v.index] = v
        end
      end
    end
    local vkey = epsilon_closure_key(v_closure)
    if vkey then
      local v = closure_to_state(v_closure, states)
      if not vmap[vkey] then
        vmap[vkey] = true
        if not color[vkey] then
          nfa_to_dfa_impl(v_closure, v, epsilon_closures, states, color)
        end
      end

      local tkey = vkey
      if resolved.action then
        tkey = vkey .. ";" .. resolved.action
      end
      local t = tmap[tkey]
      if t then
        t:update(resolved.timestamp, byte)
      else
        tmap[tkey] = transition(u, v, { [byte] = true }, resolved.timestamp, resolved.action)
      end
    end
  end

  color[ukey] = 2
end

local function nfa_to_dfa(u)
  update_state_indices(u)
  local epsilon_closures = {}
  local states = {}
  local u_closure = epsilon_closure(u, epsilon_closures)
  local u = closure_to_state(u_closure, states)
  nfa_to_dfa_impl(u_closure, u, epsilon_closures, states, {})
  return u
end

---------------------------------------------------------------------------

local function create_initial_partitions(u, accept_partition_map, nonaccept_partition, partition_map, color)
  color[u] = 1

  local partition = nonaccept_partition
  if u.accept_action then
    partition = select(2, accept_partition_map:insert_or_update(u.accept_action, function () return {} end))
  end
  -- partition:append(u)
  append(partition, u)
  partition_map[u] = partition

  for _, t in ipairs(u.transitions) do
    if not color[t.v] then
      create_initial_partitions(t.v, accept_partition_map, nonaccept_partition, partition_map, color)
    end
  end

  color[u] = 2
end

local function minimize(u)
  local accept_partition_map = tree_map()
  local partition = {}
  local partition_map = {}
  create_initial_partitions(u, accept_partition_map, partition, partition_map, {})

  local partitions = {}
  for _, partition in accept_partition_map:pairs() do
    append(partitions, partition)
  end
  if next(partition) then
    append(partitions, partition)
  end

  while true do
    local new_partition_map = {}
    local new_partitions = {}

    for _, partition in ipairs(partitions) do
      -- パーティション内の状態の組(x,y)について同じ遷移をするか調べる。同じ遷
      -- 移をする場合、ひとつのパーティションにまとめる。
      for i, x in ipairs(partition) do
        for j = 1, i - 1 do
          local y = partition[j]
          -- 全ての文字について下記の条件が満たされていたら、同じ遷移をするとみ
          -- なす。
          -- 1. 遷移先の状態が同じパーティションに含まれている。
          -- 2. 同じ遷移アクションを持つ。
          local same_transition = true
          for byte = 0x00, 0xFF do
            local x_to, _, x_action = x:simulate(byte)
            local y_to, _, y_action = y:simulate(byte)
            if partition_map[x_to] ~= partition_map[y_to] or compare(x_action, y_action) ~= 0 then
              same_transition = false
              break
            end
          end

          if same_transition then
            local new_partition = new_partition_map[x]
            if not new_partition then
              local new_partition = new_partition_map[y]
              append(new_partition, x)
              new_partition_map[x] = new_partition
            else
              -- xがすでに新パーティションに登録されている。つまり、yよりも先に
              -- 処理された状態zについて、状態の組(x,z)がひとつのパーティション
              -- にまとめられた。このとき、状態yも同じパーティションにまとめら
              -- れているはずである。
              assert(new_partition == new_partition_map[y])
            end
          end
        end

        if not new_partition_map[x] then
          local new_partition = { x }
          new_partition_map[x] = new_partition
          append(new_partitions, new_partition)
        end
      end
    end

    if #partitions == #new_partitions then
      break
    end

    partition_map = new_partition_map
    partitions = new_partitions
  end

  local states = {}
  local accept_states = {}

  for i, partition in ipairs(partitions) do
    local u = state()
    u.index = i
    for _, x in ipairs(partition) do
      u:update(x.timestamp, x.accept_action)
    end
    states[partition] = u
    if u.accept_action then
      append(accept_states, u)
    end
  end

  for i, partition in ipairs(partitions) do
    local u = states[partition]
    local transition_map = tree_map()

    for byte = 0x00, 0xFF do
      local resolved = {}
      local x_to, _, x_action = partition[1]:simulate(byte, resolved)
      if x_to then
        local p = partition_map[x_to]

        for j = 2, #partition do
          local y_to, _, y_action = partition[j]:simulate(byte, resolved)
          assert(p == partition_map[y_to])
          assert(compare(x_action, y_action) == 0)
        end

        local v = states[p]
        transition_map:insert_or_update({ index = v.index, action = resolved.action }, function ()
          return transition(u, v, { [byte] = true }, resolved.timestamp, resolved.action)
        end, function (t)
          return t:update(resolved.timestamp, byte)
        end)
      end
    end
  end

  return states[partition_map[u]], accept_states
end

---------------------------------------------------------------------------

local function collect_living_states(u, living_states, color)
  color[u] = 1

  if u.accept_action then
    living_states[u] = true
  end

  for _, t in ipairs(u.transitions) do
    if not color[t.v] then
      collect_living_states(t.v, living_states, color)
    end
    if living_states[t.v] then
      living_states[u] = true
    end
  end

  color[u] = 2
end

local function remove_dead_states(u)
  local living_states = {}
  collect_living_states(u, living_states, {})

  for v in pairs(living_states) do
    local new_transitions = {}
    for _, t in ipairs(v.transitions) do
      if living_states[t.v] then
        append(new_transitions, t)
      end
    end
    v.transitions = new_transitions
  end

  return u
end

local function simulate(u, byte, resolved_timestamp, null)
  local v, timestamp, action = u:simulate(byte)
  if not v then
    return null, resolved_timestamp
  elseif not resolved_timestamp then
    resolved_timestamp = timestamp
  end
  return v, resolved_timestamp, action
end

local function difference_impl(x, y)
  local x_states = update_state_indices(x)
  local y_states = update_state_indices(y)

  local null = state()
  null.index = 0

  local x_n = #x_states
  local y_n = #y_states
  local n = y_n + 1

  local z_states = {}
  for i = 0, x_n do
    local x = i == 0 and null or x_states[i]
    for j = i == 0 and 1 or 0, y_n do
      local y = j == 0 and null or y_states[j]
      local z = state()
      if not y.accept_action then
        z:update(x.timestamp, x.accept_action)
      end
      z_states[i * n + j] = z
    end
  end

  for i = 0, x_n do
    local x_u = i == 0 and null or x_states[i]

    for j = i == 0 and 1 or 0, y_n do
      local y_u = j == 0 and null or y_states[j]
      local z_u = z_states[i * n + j]

      local tmap = {}
      for byte = 0x00, 0xFF do
        local x_v, timestamp, action = simulate(x_u, byte, nil, null)
        local y_v, timestamp = simulate(y_u, byte, timestamp, null)
        local index = x_v.index * n + y_v.index
        if index ~= 0 then
          local z_v = z_states[index]
          local tkey = index
          if action then
            tkey = index .. ";" .. action
          end
          local t = tmap[tkey]
          if t then
            t:update(timestamp, byte)
          else
            tmap[tkey] = transition(z_u, z_v, {}, timestamp, action)
          end
        end
      end
    end
  end

  return z_states[x.index * n + y.index]
end

function difference(x, y)
  return minimize(remove_dead_states(difference_impl(minimize(nfa_to_dfa(x)), minimize(nfa_to_dfa(y)))))
end

---------------------------------------------------------------------------

local module = {}

local function machine(timestamp, s)
  local start_state, accept_states = minimize(nfa_to_dfa(s))
  return { timestamp = timestamp, start_state = start_state, accept_states = accept_states }
end

function module.union(that)
  local s = state()
  for _, node in ipairs(that) do
    transition(s, (tree_to_nfa(node)))
  end
  return machine(that[1].timestamp, s)
end

function module.guard(guard_action, that)
  local self = module.union(that)
  self.guard_action = guard_action
  return self
end

function module.lexer(token_names, that)
  local data = {}
  for name, node in pairs(that) do
    if type(name) ~= "string" then
      name = node.literal
    end
    local timestamp = node.timestamp
    assert(timestamp)
    append(data, { timestamp = timestamp, node = node, name = name })
  end
  table.sort(data, function (a, b) return a.timestamp < b.timestamp end)

  local token_table = {}
  for symbol, name in ipairs(token_names) do
    token_table[name] = symbol
  end

  local s = state()
  for _, item in ipairs(data) do
    local u, v = tree_to_nfa(item.node)
    transition(s, u)

    local symbol = "nil"
    if item.name then
      symbol = token_table[item.name]
      if not symbol then
        symbol = append(token_names, item.name)
        token_table[item.name] = symbol
      end
    end

    if v.accept_action == "" then
      v.accept_action = "ts=" .. symbol .. " push()"
    else
      v.accept_action = "ts=" .. symbol .. ";" .. v.accept_action
    end
  end

  return machine(data[1].timestamp, s)
end

return module
