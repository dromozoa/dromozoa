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

local array = require "dromozoa.array"
local compare = require "dromozoa.compare"
local tree_map = require "dromozoa.tree_map"

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.regexp.machine.state" }

function class:simulate(byte, resolved)
  for _, t in self.transitions:ipairs() do
    if t.set ~= nil and t.set[byte] then
      if resolved ~= nil and (resolved.timestamp == nil or resolved.timestamp > t.timestamp) then
        resolved.timestamp = t.timestamp
        resolved.action = t.action
      end
      return t.v, t.timestamp, t.action
    end
  end
end

function class:update(timestamp, accept_action)
  if timestamp ~= nil and accept_action ~= nil and (self.timestamp == nil or self.timestamp > timestamp) then
    self.timestamp = timestamp
    self.accept_action = accept_action
  end
  return self
end

local function state()
  return setmetatable({ transitions = array() }, metatable)
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.regexp.machine.transition" }

function class:update(timestamp, byte)
  if self.timestamp > timestamp then
    self.timestamp = timestamp
  end
  self.set[byte] = byte
  return self
end

local function transition(u, v, set, timestamp, action)
  local self = setmetatable({ v = v, set = set, timestamp = timestamp, action = action }, metatable)
  u.transitions:append(self)
  return self
end

---------------------------------------------------------------------------

local difference

local function node_to_nfa(node)
  local timestamp = rawget(node, "timestamp")
  assert(timestamp ~= nil)

  local code = node[0]
  if code == "[" then
    local u = state()
    local v = state()
    transition(u, v, node[1], timestamp)
    return u, v
  else
    local au, av = node_to_nfa(node[1])
    if code == "/" then
      au.transitions:get(1).action = node[2]
      return au, av
    elseif code == "%" then
      av:update(timestamp, node[2])
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
          av:update(timestamp, "")
          bv:update(timestamp, "")
          local cu, accept_states = difference(au, bu)
          transition(u, cu)
          for _, cv in accept_states:ipairs() do
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
  if v.accept_action == nil then
    v:update(rawget(node, "timestamp"), "")
  end
  return u, v
end

---------------------------------------------------------------------------

local function update_state_indices_impl(u, states, color)
  color[u] = 1
  u.index = states:append(u):size()
  for _, t in u.transitions:ipairs() do
    if color[t.v] == nil then
      update_state_indices_impl(t.v, states, color)
    end
  end
  color[u] = 2
end

local function update_state_indices(u)
  local states = array()
  update_state_indices_impl(u, states, {})
  return states
end

---------------------------------------------------------------------------

local function epsilon_closure_impl(u, closure)
  for _, t in u.transitions:ipairs() do
    if t.set == nil then
      closure:insert(t.v.index, t.v)
      epsilon_closure_impl(t.v, closure)
    end
  end
end

local function epsilon_closure(u, epsilon_closures)
  local closure = epsilon_closures[u]
  if closure == nil then
    closure = tree_map():insert(u.index, u)
    epsilon_closure_impl(u, closure)
    epsilon_closures[u] = closure
  end
  return closure
end

local function closure_to_state(closure, states)
  return select(2, states:insert_or_update(closure, function ()
    local u = state()
    for _, v in closure:pairs() do
      u:update(v.timestamp, v.accept_action)
    end
    return u
  end))
end

local function nfa_to_dfa_impl(u_closure, u, epsilon_closures, states, color)
  color:assign(u_closure, 1)

  local state_map = tree_map()
  local transition_map = tree_map()

  for byte = 0x00, 0xFF do
    local resolved = {}
    local v_closure = tree_map()
    for _, u in u_closure:pairs() do
      local to = u:simulate(byte, resolved)
      if to ~= nil then
        for k, v in epsilon_closure(to, epsilon_closures):pairs() do
          v_closure:assign(k, v)
        end
      end
    end

    if not v_closure:empty() then
      local v = closure_to_state(v_closure, states)
      state_map:assign(v_closure, v)
      transition_map:insert_or_update({ closure = v_closure, action = resolved.action }, function ()
        return transition(u, v, { [byte] = true }, resolved.timestamp, resolved.action)
      end, function (t)
        return t:update(resolved.timestamp, byte)
      end)
    end
  end

  for v_closure, v in state_map:pairs() do
    if color:find(v_closure) == nil then
      nfa_to_dfa_impl(v_closure, v, epsilon_closures, states, color)
    end
  end

  color:assign(u_closure, 2)
end

local function nfa_to_dfa(u)
  update_state_indices(u)
  local epsilon_closures = {}
  local states = tree_map()
  local u_closure = epsilon_closure(u, epsilon_closures)
  local u = closure_to_state(u_closure, states)
  nfa_to_dfa_impl(u_closure, u, epsilon_closures, states, tree_map())
  return u
end

---------------------------------------------------------------------------

local function create_initial_partitions(u, accept_partition_map, nonaccept_partition, partition_map, color)
  color[u] = 1

  local partition = nonaccept_partition
  if u.accept_action ~= nil then
    partition = select(2, accept_partition_map:insert_or_update(u.accept_action, array))
  end
  partition:append(u)
  partition_map[u] = partition

  for _, t in u.transitions:ipairs() do
    if color[t.v] == nil then
      create_initial_partitions(t.v, accept_partition_map, nonaccept_partition, partition_map, color)
    end
  end

  color[u] = 2
end

local function minimize(u)
  local accept_partition_map = tree_map()
  local partition = array()
  local partition_map = {}
  create_initial_partitions(u, accept_partition_map, partition, partition_map, {})

  local partitions = array()
  local partition_indices = {}
  for _, partition in accept_partition_map:pairs() do
    partition_indices[partition] = partitions:append(partition):size()
  end
  if not partition:empty() then
    partition_indices[partition] = partitions:append(partition):size()
  end

  while true do
    local new_partition_map = {}
    local new_partitions = array()
    local new_partition_indices = {}

    for _, partition in partitions:ipairs() do
      -- パーティション内の状態の組(x,y)について同じ遷移をするか調べる。同じ遷
      -- 移をする場合、ひとつのパーティションにまとめる。
      for i, x in partition:ipairs() do
        for j = 1, i - 1 do
          local y = partition:get(j)
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
            if new_partition == nil then
              local new_partition = new_partition_map[y]
              new_partition:append(x)
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

        if new_partition_map[x] == nil then
          local new_partition = array(x)
          new_partition_map[x] = new_partition
          new_partition_indices[new_partition] = new_partitions:append(new_partition):size()
        end
      end
    end

    if partitions:size() == new_partitions:size() then
      break
    end

    partition_map = new_partition_map
    partitions = new_partitions
    partition_indices = new_partition_indices
  end

  local states = {}
  local accept_states = array()

  for i, partition in partitions:ipairs() do
    local u = state()
    for _, x in partition:ipairs() do
      u:update(x.timestamp, x.accept_action)
    end
    states[partition] = u
    if u.accept_action ~= nil then
      accept_states:append(u)
    end
  end

  for i, partition in partitions:ipairs() do
    local u = states[partition]
    local transition_map = tree_map()

    for byte = 0x00, 0xFF do
      local resolved = {}
      local x_to, _, x_action = partition:get(1):simulate(byte, resolved)
      if x_to ~= nil then
        local p = partition_map[x_to]

        for j = 2, partition:size() do
          local y_to, _, y_action = partition:get(j):simulate(byte, resolved)
          assert(p == partition_map[y_to])
          assert(compare(x_action, y_action) == 0)
        end

        local v = states[p]
        transition_map:insert_or_update({ index = partition_indices[p], action = resolved.action }, function ()
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

  if u.accept_action ~= nil then
    living_states[u] = true
  end

  for _, t in u.transitions:ipairs() do
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
    local new_transitions = array()
    for _, t in v.transitions:ipairs() do
      if living_states[t.v] then
        new_transitions:append(t)
      end
    end
    v.transitions = new_transitions
  end

  return u
end

local function simulate(u, byte, resolved_timestamp, null)
  local v, timestamp, action = u:simulate(byte)
  if v == nil then
    return null, resolved_timestamp
  elseif resolved_timestamp == nil then
    resolved_timestamp = timestamp
  end
  return v, resolved_timestamp, action
end

local function difference_impl(x, y)
  local x_states = update_state_indices(x)
  local y_states = update_state_indices(y)

  local null = state()
  null.index = 0

  local x_n = x_states:size()
  local y_n = y_states:size()
  local n = y_n + 1

  local z_states = {}
  for i = 0, x_n do
    local x = i == 0 and null or x_states:get(i)
    for j = i == 0 and 1 or 0, y_n do
      local y = j == 0 and null or y_states:get(j)
      local z = state()
      if y.accept_action == nil then
        z:update(x.timestamp, x.accept_action)
      end
      z_states[i * n + j] = z
    end
  end

  for i = 0, x_n do
    local x_u = i == 0 and null or x_states:get(i)

    for j = i == 0 and 1 or 0, y_n do
      local y_u = j == 0 and null or y_states:get(j)
      local z_u = z_states[i * n + j]

      local transition_map = tree_map()
      for byte = 0x00, 0xFF do
        local x_v, timestamp, action = simulate(x_u, byte, nil, null)
        local y_v, timestamp = simulate(y_u, byte, timestamp, null)
        local index = x_v.index * n + y_v.index
        if index ~= 0 then
          local z_v = z_states[index]
          transition_map:insert_or_update({ index = index, action = action }, function ()
            return transition(z_u, z_v, {}, timestamp, action)
          end, function (t)
            return t:update(timestamp, byte)
          end)
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
local metatable = { __name = "dromozoa.regexp.machine" }

local function machine(timestamp, start_state)
  return setmetatable({ timestamp = timestamp, start_state = start_state }, metatable)
end

function module.union(that)
  local s = state()
  for _, node in ipairs(that) do
    transition(s, (tree_to_nfa(node)))
  end
  return machine(rawget(that[1], "timestamp"), minimize(nfa_to_dfa(s)))
end

function module.guard(guard_action, that)
  local self = module.union(that)
  self.guard_action = guard_action
  return self
end

function module.lexer(token_names, that)
  local data = array()
  for name, node in pairs(that) do
    if type(name) ~= "string" then
      name = rawget(node, "literal")
    end
    local timestamp = rawget(node, "timestamp")
    if timestamp == nil then
      error "pattern has no timestamp"
    end
    data:append { timestamp = timestamp, node = node, name = name }
  end
  data:sort(function (a, b) return a.timestamp < b.timestamp end)

  -- TODO 複数のレキサを定義できるか？
  local token_table = {}
  for symbol, name in token_names:ipairs() do
    token_table[name] = symbol
  end

  local s = state()
  for _, item in data:ipairs() do
    local u, v = tree_to_nfa(item.node)
    transition(s, u)

    local symbol = "nil"
    if item.name ~= nil then
      symbol = token_table[item.name]
      if symbol == nil then
        symbol = token_names:append(item.name):size()
        token_table[item.name] = symbol
      end
    end

    if v.accept_action == "" then
      v.accept_action = "tk=" .. symbol .. " push()"
    else
      v.accept_action = "tk=" .. symbol .. ";" .. v.accept_action
    end
  end

  return machine(data:get(1).timestamp, minimize(nfa_to_dfa(s)))
end

return module
