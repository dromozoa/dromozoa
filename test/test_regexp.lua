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

local dumper = require "dromozoa.commons.dumper"
local write_graphviz = require "dromozoa.regexp.write_graphviz"
local write_graphviz_tree = require "dromozoa.regexp.write_graphviz_tree"
local tree_map = require "dromozoa.tree_map"

local module = {}

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.regexp.pattern" }

local timestamp = 0

local function construct(code, ...)
  timestamp = timestamp + 1
  return setmetatable({ timestamp = timestamp, [0] = code, ... }, metatable)
end

function metatable:__add(that)
  local self = module.pattern(self)
  local that = module.pattern(that)
  if self[0] == "%" or that[0] == "%" then
    error "not supported"
  else
    return construct(".", self, that)
  end
end

function metatable:__sub(that)
  local self = module.pattern(self)
  local that = module.pattern(that)
  if self[0] == "%" or that[0] == "%" then
    error "not supported"
  elseif self[0] == "[" and that[0] == "[" then
    local sub = that[1]
    local set = {}
    for byte in pairs(self[1]) do
      if not sub[byte] then
        set[byte] = true
      end
    end
    return construct("[", set)
  else
    return construct("-", self, that)
  end
end

function metatable:__div(that)
  local self = module.pattern(self)
  if self[0] == "[" then
    return construct("/", self, that)
  else
    error "not supported"
  end
end

function metatable:__mod(that)
  local self = module.pattern(self)
  if self[0] == "%" then
    error "not supported"
  else
    local result = construct("%", self, that)
    result.name = self.name
    return result
  end
end

function metatable:__bor(that)
  local self = module.pattern(self)
  local that = module.pattern(that)
  if self[0] == "%" or that[0] == "%" then
    error "not supported"
  elseif self[0] == "[" and that[0] == "[" then
    local set = {}
    for byte in pairs(self[1]) do
      set[byte] = true
    end
    for byte in pairs(that[1]) do
      set[byte] = true
    end
    return construct("[", set)
  else
    return construct("|", self, that)
  end
end

function metatable:__bnot(that)
  if self[0] == "[" then
    local neg = self[1]
    local set = {}
    for byte = 0x00, 0xFF do
      if not neg[byte] then
        set[byte] = true
      end
    end
    return construct("[", set)
  else
    error "not supported"
  end
end

function metatable:__call(that)
  if self[0] == "%" then
    error "not supported"
  else
    local m = that[1]
    local n = that[2]
    if n == nil then
      if m == 0 then
        return construct("*", self)
      elseif m == 1 then
        return construct("+", self)
      else
        local result = self
        for i = 3, m do
          result = result + self
        end
        return result + construct("+", self)
      end
    else
      if m == 0 then
        local result = construct("?", self)
        for i = 2, n do
          result = result + construct("?", self)
        end
        return result
      else
        local result = self
        for i = 2, m do
          result = result + self
        end
        for i = m + 1, n do
          result = result + construct("?", self)
        end
        return result
      end
    end
  end
end

function module.pattern(that)
  local t = type(that)
  if t == "string" then
    local self = construct("[", { [that:byte(1)] = true })
    for i = 2, #that do
      self = self + construct("[", { [that:byte(i)] = true })
    end
    self.name = that
    return self
  else
    return that
  end
end

function module.pattern_range(that)
  local set = {}
  for i = 1, #that, 2 do
    local a, b = that:byte(i, i + 1)
    for byte = a, b do
      set[byte] = true
    end
  end
  return construct("[", set)
end

function module.pattern_set(that)
  local set = {}
  for i = 1, #that do
    set[that:byte(i)] = true
  end
  return construct("[", set)
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.regexp.pattern.constructor" }

function metatable:__index(that)
  return module.pattern_range(that)
end

function metatable:__call(that)
  if type(that) == "table" then
    return module.pattern_set(that[1])
  else
    return module.pattern(that)
  end
end

module.constructor = setmetatable({}, metatable)

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.regexp.list" }

function class:append(...)
  local n = #self
  for i = 1, select("#", ...) do
    self[n + i] = select(i, ...)
  end
  return self
end

function class:slice(i, j)
  return module.list(table.unpack(self, i, j))
end

function module.list(...)
  return setmetatable({...}, metatable)
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.regexp.transition" }

function class:update(byte, timestamp)
  self.set[byte] = byte
  if self.timestamp > timestamp then
    self.timestamp = timestamp
  end
end

function module.transition(v, set, timestamp, action)
  return setmetatable({ v = v, set = set, timestamp = timestamp, action = action }, metatable)
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.regexp.state" }

-- TODO new_transitionのほうがよいか？
-- TODO transitionの構造はこれでいいのか？
--  とりあえずはこれで
function class:transition(v, set, timestamp, action)
  local transition = module.transition(v, set, timestamp, action)
  self.transitions:append(transition)
  return transition
end

-- 0x00..0xFFのくりかえしでしか呼ばれない
function class:execute_transition(byte)
  for _, transition in ipairs(self.transitions) do
    local set = transition.set
    if set and set[byte] then
      return transition.v, transition.timestamp, transition.action
    end
  end
end

function module.state()
  return setmetatable({ transitions = module.list() }, metatable)
end

---------------------------------------------------------------------------

-- TODO timestampは一括でつけてよいかもしれない
-- TODO stateに一意なtimestampをつけるのはやめる
-- TODO stateに必要なのはpointer/handleのようなIDかな
local function node_to_nfa(node)
  local code = node[0]
  if code == "[" then
    local u = module.state()
    local v = module.state()
    u:transition(v, node[1], timestamp)
    return u, v
  else
    local au, av = node_to_nfa(node[1])
    if code == "." then
      local bu, bv = node_to_nfa(node[2])
      av:transition(bu)
      return au, bv
    elseif code == "|" then
      local bu, bv = node_to_nfa(node[2])
      local u = module.state()
      local v = module.state()
      u:transition(au)
      u:transition(bu)
      av:transition(v)
      bv:transition(v)
      return u, v
    elseif code == "*" then
      local u = module.state()
      local v = module.state()
      u:transition(v)
      u:transition(au)
      av:transition(au)
      av:transition(v)
      return u, v
    elseif code == "+" then
      local u = module.state()
      local v = module.state()
      u:transition(au)
      av:transition(au)
      av:transition(v)
      return u, v
    elseif code == "?" then
      local u = module.state()
      local v = module.state()
      u:transition(v)
      u:transition(au)
      av:transition(v)
      return u, v
    elseif code == "-" then
      local bu, bv = node_to_nfa(node[2])
      local u = module.state()
      local v = module.state()

      -- 差集合を求めるためにaccept_actionとtimestampを割り当てる
      local timestamp = node.timestamp
      au.timestamp = timestamp
      av.accept_action = true
      av.timestamp = timestamp
      bu.timestamp = timestamp
      bv.accept_action = true
      bv.timestamp = timestamp

      local cu, accept_states = module.minimize(
        module.difference(
          module.minimize(module.nfa_to_dfa(au)),
          module.minimize(module.nfa_to_dfa(bu))))

      -- 求めた差集合からaccept_actionとtimestampを除去する
      cu.timestamp = nil
      u:transition(cu)
      for _, cv in ipairs(accept_states) do
        cv.accept_action = nil
        cv.timestamp = nil
        cv:transition(v)
      end

      return u, v
    elseif code == "/" then
      assert(#au.transitions == 1)
      au.transitions[1].action = node[2]
      return au, av
    elseif code == "%" then
      assert(#av.transitions == 0)
      av.accept_action = node[2]
      av.timestamp = node.timestamp
      return au, av
    end
  end
end

-- TODO accept_actionのnilの扱いを検討する
function module.tree_to_nfa(root, accept_action)
  local u, v = node_to_nfa(root)
  local timestamp = root.timestamp
  u.timestamp = timestamp
  if not v.accept_action then
    v.accept_action = accept_action or true
    v.timestamp = timestamp
  end
  return u, v
end

---------------------------------------------------------------------------

local function update_state_indices(u, states, color)
  color[u] = 1
  u.index = #states:append(u)
  for _, transition in ipairs(u.transitions) do
    if not color[transition.v] then
      index = update_state_indices(transition.v, states, color)
    end
  end
  color[u] = 2
  return index
end

function module.update_state_indices(u)
  local states = module.list()
  update_state_indices(u, states, {})
  return states
end

---------------------------------------------------------------------------

local function epsilon_closure(u, map)
  for _, transition in ipairs(u.transitions) do
    if not transition.set then
      map[transition.v.index] = transition.v
      epsilon_closure(transition.v, map)
    end
  end
end

function module.epsilon_closure(u, epsilon_closures)
  local map = epsilon_closures[u]
  if not map then
    map = tree_map()
    map[u.index] = u
    epsilon_closure(u, map)
    epsilon_closures[u] = map
  end
  return map
end

local function new_state(map)
  local accept_action
  local timestamp

  for _, u in pairs(map) do
    if u.accept_action then
      if not timestamp or timestamp > u.timestamp then
        accept_action = u.accept_action
        timestamp = u.timestamp
      end
    end
  end

  local state = module.state()
  state.accept_action = accept_action
  state.timestamp = timestamp
  return state
end

local function nfa_to_dfa(umap, unew, states, epsilon_closures, color)
  color[umap] = 1

  local actions = {}

  local new_transition_map = tree_map()
  local new_states = module.list()

  for byte = 0x00, 0xFF do
    local vmap = tree_map()
    local action
    local timestamp

    -- 複数のノードについて、遷移を調べる
    for _, u in pairs(umap) do
      local transition_v, transition_timestamp, transition_action = u:execute_transition(byte)
      if transition_v then
        if not timestamp or timestamp > transition_timestamp then
          action = transition_action
          timestamp = transition_timestamp
        end
        for k, v in pairs(module.epsilon_closure(transition_v, epsilon_closures)) do
          vmap[k] = v
        end
      end
    end

    if vmap():next() then
      local vnew = states[vmap]
      if not vnew then
        vnew = new_state(vmap)
        states[vmap] = vnew
        new_states:append { map = vmap, state = vnew }
      end

      -- 遷移先とアクションがいっしょの遷移をさがす
      local new_transition_key = { map = vmap, action = action }
      local new_transition = new_transition_map[new_transition_key]
      if not new_transition then
        new_transition_map[new_transition_key] = unew:transition(vnew, { [byte] = true }, timestamp, action)
      else
        new_transition:update(byte, timestamp)
      end
    end
  end

  for _, item in ipairs(new_states) do
    if not color[item.map] then
      nfa_to_dfa(item.map, item.state, states, epsilon_closures, color)
    end
  end

  color[umap] = 2
end

function module.nfa_to_dfa(u)
  module.update_state_indices(u)
  local epsilon_closures = {}

  local umap = module.epsilon_closure(u, epsilon_closures)
  local unew = new_state(umap)

  local states = tree_map()
  local color = tree_map()
  states[umap] = unew

  nfa_to_dfa(umap, unew, states, epsilon_closures, color)
  unew.timestamp = u.timestamp
  return unew
end

---------------------------------------------------------------------------

local assertion = true

local function create_initial_partitions(u, accept_partition_map, nonaccept_partition, partition_map, color)
  color[u] = 1

  local accept_action = u.accept_action
  local partition = nonaccept_partition
  if accept_action then
    partition = accept_partition_map[accept_action]
    if not partition then
      partition = module.list()
      partition.timestamp = u.timestamp
      accept_partition_map[accept_action] = partition
    else
      if partition.timestamp > u.timestamp then
        partition.timestamp = u.timestamp
      end
    end
  end
  partition:append(u)
  partition_map[u] = partition

  for _, transition in ipairs(u.transitions) do
    if not color[transition.v] then
      create_initial_partitions(transition.v, accept_partition_map, nonaccept_partition, partition_map, color)
    end
  end

  color[u] = 2
end

function module.create_initial_partitions(u)
  local accept_partition_map = tree_map()
  local nonaccept_partition = module.list()
  local partition_map = {}
  create_initial_partitions(u, accept_partition_map, nonaccept_partition, partition_map, {})

  local partitions = module.list()
  for _, partition in pairs(accept_partition_map) do
    partitions:append(partition)
  end
  table.sort(partitions, function (a, b) return a.timestamp < b.timestamp end)

  if #nonaccept_partition > 0 then
    partitions:append(nonaccept_partition)
  end

  return partitions, partition_map
end

function module.minimize(u)
  local partitions, partition_map = module.create_initial_partitions(u)

  while true do
    local new_partitions = module.list()
    local new_partition_map = {}

    for _, partition in ipairs(partitions) do
      -- あるパーティションに含まれる状態の組 (x, y) が同じ遷移をするならば、ひ
      -- とつのパーティションにまとめる。
      for i = 1, #partition do
        local x = partition[i]
        for j = 1, i - 1 do
          local y = partition[j]
          -- 全ての文字について下記の条件が満たされたら、同じ遷移をするとみなす。
          -- 1. 遷移先の状態が同じパーティションに含まれている
          -- 2. 同じ遷移アクションを持つ
          local same_partition = true
          for byte = 0x00, 0xFF do
            local xv, _, xaction = x:execute_transition(byte)
            local yv, _, yaction = y:execute_transition(byte)
            -- TODO compare(xaction, yaction) == 0 にするべきか？
            if partition_map[xv] ~= partition_map[yv] or xaction ~= yaction then
              same_partition = false
              break
            end
          end

          if same_partition then
            local new_partition = new_partition_map[x]
            if not new_partition then
              local new_partition = new_partition_map[y]
              new_partition:append(x)
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
          local new_partition = module.list(x)
          new_partitions:append(new_partition)
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

    local unew = module.state()
    unew.accept_action = accept_action
    unew.timestamp = timestamp
    states[partition] = { index = i, state = unew }
    if accept_action then
      accept_states[#accept_states + 1] = unew
    end
  end

  for i = 1, #partitions do
    local partition = partitions[i]
    local unew = states[partition].state

    local actions = {}
    local new_transition_map = tree_map()

    for byte = 0x00, 0xFF do
      local transition_v, transition_timestamp, transition_action = partition[1]:execute_transition(byte)

      if transition_v then
        local timestamp = transition_timestamp
        local action = transition_action
        local v = states[partition_map[transition_v]]

        -- パーティションに含まれる各状態は同じ遷移をする
        for j = 2, #partition do
          local transition_v, transition_timestamp, transition_action = partition[j]:execute_transition(byte)
          if assertion then
            -- TODO compareにするべき？
            assert(action == transition_action)
            assert(v.index == states[partition_map[transition_v]].index)
            assert(v.state == states[partition_map[transition_v]].state)
          end
          local t = transition_timestamp
          if timestamp > t then
            timestamp = t
          end
        end

        local new_transition_key = { index = v.index, action = action }
        local new_transition = new_transition_map[new_transition_key]
        if not new_transition then
          new_transition_map[new_transition_key] = unew:transition(v.state, { [byte] = true }, timestamp, action)
        else
          if assertion then
            -- TODO compareにするべき？
            assert(action == new_transition.action)
            assert(v.state == new_transition.v)
          end
          new_transition:update(byte, timestamp)
        end

      else
        if assertion then
          for j = 2, #partition do
            assert(not partition[j]:execute_transition(byte))
          end
        end
      end
    end
  end

  local unew = states[partition_map[u]].state
  unew.timestamp = u.timestamp
  return unew, accept_states
end

---------------------------------------------------------------------------

local function remove_dead_states(u, living_states, color)
  color[u] = 1

  if u.accept_action then
    living_states[u] = true
  end

  for _, transition in ipairs(u.transitions) do
    local v = transition.v
    if not color[v] then
      remove_dead_states(v, living_states, color)
    end
    -- 帰りがけに調べる
    if living_states[v] then
      living_states[u] = true
    end
  end

  color[u] = 2
end

function module.remove_dead_states(u)
  local living_states = {}
  remove_dead_states(u, living_states, {})

  -- visit2(u, living_states, {})
  -- 再帰する必要はない
  for u in pairs(living_states) do
    local new_transitions = module.list()
    for _, transition in ipairs(u.transitions) do
      if living_states[transition.v] then
        new_transitions:append(transition)
      end
    end
    u.transitions = new_transitions
  end

  return u
end

---------------------------------------------------------------------------

local function new_state(ux, uy)
  local state = module.state()
  if ux then
    local accept_action = ux.accept_action
    if accept_action and (not uy or not uy.accept_action) then
      state.accept_action = accept_action
      state.timestamp = ux.timestamp
    end
  end
  return state
end

function module.difference(ux, uy)
  local x_states = module.update_state_indices(ux)
  local y_states = module.update_state_indices(uy)

  local nx = #x_states
  local ny = #y_states
  local n = nx + 1

  local new_states = {}

  for i = 0, nx do
    local ux = x_states[i]
    for j = 0, ny do
      local uy = y_states[j]
      local ukey = i + j * n
      if ukey ~= 0 then
        local new_transition_map = tree_map()

        for byte = 0x00, 0xFF do
          local vx, tx, action
          if ux then
            vx, tx, action = ux:execute_transition(byte)
          end
          local vy, ty
          if uy then
            vy, ty = uy:execute_transition(byte)
          end

          local vkey = 0
          local timestamp
          if ty then
            vkey = vy.index * n
            timestamp = ty
          end
          if tx then
            vkey = vx.index + vkey
            timestamp = tx
          end

          if vkey ~= 0 then
            local unew = new_states[ukey]
            if not unew then
              unew = new_state(ux, uy)
              new_states[ukey] = unew
            end
            local vnew = new_states[vkey]
            if not vnew then
              vnew = new_state(vx, vy)
              new_states[vkey] = vnew
            end

            local new_transition_key = { index = vkey, action = action }
            local new_transition = new_transition_map[new_transition_key]
            if not new_transition then
              new_transition_map[new_transition_key] = unew:transition(vnew, { [byte] = true }, timestamp, action)
            else
              new_transition:update(byte, timestamp)
            end
          end
        end
      end
    end
  end

  local unew = new_states[ux.index + uy.index * n]
  unew.timestamp = ux.timestamp
  return module.remove_dead_states(unew)
end

---------------------------------------------------------------------------

local _ = module.constructor

local x = (_"a"{0} + _"b"{1} + (_"c"/"T"){0,1} - "abc" | _{"xyz"}{3,3}) %"A"
local d = module.nfa_to_dfa(module.tree_to_nfa(x, true))

local out = assert(io.open("test.dot", "w"))
write_graphviz(out, d)
out:close()
