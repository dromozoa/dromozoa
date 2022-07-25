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
--
-- https://github.com/aidansteele/osx-abi-macho-file-format-reference
-- https://developers.wonderpla.net/entry/2021/03/19/105503

local dumper = require "dromozoa.commons.dumper"
local write_graphviz = require "dromozoa.regexp.write_graphviz"
local write_graphviz_tree = require "dromozoa.regexp.write_graphviz_tree"

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
local metatable = { __index = class, __name = "dromozoa.regexp.state" }

--TODO new_transitionのほうがよいか？
function class:transition(v, set, action)
  local transition = { v = v, set = set, action = action }
  self.transitions:append(transition)
  return transition
end

function class:execute_transition(byte)
  for _, transition in ipairs(self.transitions) do
    local set = transition.set
    if set and set[byte] then
      return transition
    end
  end
end

function module.state()
  return setmetatable({ transitions = module.list() }, metatable)
end

---------------------------------------------------------------------------

-- TODO timestampは一括でつけてよいかもしれない
local function node_to_nfa(node)
  local code = node[0]
  if code == "[" then
    local u = module.state()
    local v = module.state()
    u:transition(v, node[1]).timestamp = node.timestamp
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
--[[

  挿入順序でつくるmapは簡単
  キーが一定の順序になるのは、RB木やソートが必要？
  pairsかkeyを呼び出した時点でソートするとか？

  欲しいのは状態の集合
  { index, state }をputする
]]--
---------------------------------------------------------------------------

-- 深さ優先探索で状態に番号をつける
local function create_state_indices(u, indices, index, color)
  color[u] = 1
  index = index + 1
  indices[u] = index

  for _, transition in ipairs(u.transitions) do
    if not color[transition.v] then
      index = create_state_indices(transition.v, indices, index, color)
    end
  end

  color[u] = 2
  return index
end

function module.create_state_indices(u)
  local indices = {}
  create_state_indices(u, indices, 0, {})
  return indices
end

-- epsilon遷移をたどって閉包を作成する
-- map<index, state>
local function epsilon_closure(u, map, indices)
  for _, transition in ipairs(u.transitions) do
    if not transition.set then
      map[indices[transition.v]] = transition.v
      epsilon_closure(transition.v, map, indices)
    end
  end
end

local function map_to_seq(map)
  local seq = {}
  for index, state in pairs(map) do
    seq[#seq + 1] = { index = index, state = state }
  end
  table.sort(seq, function (a, b) return a.index < b.index end)
  local key = {}
  for i = 1, #seq do
    key[i] = seq[i].index
  end
  seq.key = table.concat(key, ",")
  return seq
end

function module.epsilon_closure(u, epsilon_closures, indices)
  local seq = epsilon_closures[u]
  if not seq then
    local map = { [indices[u]] = u }
    epsilon_closure(u, map, indices)
    seq = map_to_seq(map)
    epsilon_closures[u] = seq
  end
  return seq
end

local function new_state(seq)
  local accept_action
  local timestamp

  for _, x in ipairs(seq) do
    local u = x.state
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
  seq.state = state
  return state
end

local function nfa_to_dfa(useq, states, epsilon_closures, indices, color)
  local ukey = useq.key
  local unew = useq.state

  color[ukey] = 1

  local actions = {}
  local new_transition_map = {}
  local new_states = {}

  for byte = 0x00, 0xFF do
    local vmap = {}
    local action
    local timestamp

    for _, u in ipairs(useq) do
      local transition = u.state:execute_transition(byte)
      if transition then
        if not timestamp or timestamp > transition.timestamp then
          action = transition.action
          timestamp = transition.timestamp
        end
        local vseq = module.epsilon_closure(transition.v, epsilon_closures, indices)
        for _, v in ipairs(vseq) do
          vmap[v.index] = v.state
        end
      end
    end

    if next(vmap) then
      local vseq = map_to_seq(vmap)
      local vkey = vseq.key
      local vnew

      local xseq = states[vkey]
      if not xseq then
        vnew = new_state(vseq)
        states[vkey] = vseq
        new_states[#new_states + 1] = vseq
      else
        vnew = xseq.state
      end

      local new_transition_key = module.new_transition_key(vkey, actions, action)
      local new_transition = new_transition_map[new_transition_key]
      if not new_transition then
        new_transition = unew:transition(vnew, { [byte] = true })
        new_transition.action = action
        new_transition.timestamp = timestamp
        new_transition_map[new_transition_key] = new_transition
      else
        new_transition.set[byte] = true
        if new_transition.timestamp > timestamp then
          new_transition.timestamp = timestamp
        end
      end

    end
  end

  for _, vseq in ipairs(new_states) do
    if not color[vseq.key] then
      nfa_to_dfa(vseq, states, epsilon_closures, indices, color)
    end
  end

  color[ukey] = 2
end

function module.nfa_to_dfa(u)
  local indices = module.create_state_indices(u)
  local epsilon_closures = {}
  local useq = module.epsilon_closure(u, epsilon_closures, indices)
  local unew = new_state(useq)
  nfa_to_dfa(useq, { [useq.key] = useq }, epsilon_closures, indices, {})
  unew.timestamp = u.timestamp
  return unew
end

---------------------------------------------------------------------------

local private = setmetatable({}, { __mode = "k" })

function module.new_transition_key(key, actions, action)
  if action then
    local index = actions[action]
    if not index then
      index = (private[actions] or 0) + 1
      actions[action] = index
      private[actions] = index
    end
    return key .. ";" .. index
  else
    return tostring(key)
  end
end

---------------------------------------------------------------------------

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
      local t = u.timestamp
      if partition.timestamp > t then
        partition.timestamp = t
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
  table.sort(partitions, function (a, b) return a.timestamp < b.timestamp end)

  if #nonaccept_partition > 0 then
    partitions[#partitions + 1] = nonaccept_partition
  end

  return partitions, partition_map
end

local function execute_transition(u, byte)
  local transition = u:execute_transition(byte)
  if transition then
    return transition.v, transition.action
  end
end

function module.minimize(u)
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

    local unew = module.state()
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

    local actions = {}
    local new_transition_map = {}

    for byte = 0x00, 0xFF do
      local transition = partition[1]:execute_transition(byte)

      if transition then
        local timestamp = transition.timestamp
        local action = transition.action
        local v = states[partition_map[transition.v]]
        local vkey = v.key
        local vnew = v.state

        -- パーティションに含まれる各状態は同じ遷移をする
        for j = 2, #partition do
          local transition = partition[j]:execute_transition(byte)
          if assertion then
            assert(rawequal(action, transition.action))
            assert(vkey == states[partition_map[transition.v]].key)
            assert(vnew == states[partition_map[transition.v]].state)
          end
          local t = transition.timestamp
          if timestamp > t then
            timestamp = t
          end
        end

        local new_transition_key = module.new_transition_key(vkey, actions, action)
        local new_transition = new_transition_map[new_transition_key]
        if not new_transition then
          new_transition = unew:transition(vnew, { [byte] = true })
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

local function visit1(u, not_dead_states, color)
  color[u] = 1

  if u.accept_action then
    not_dead_states[u] = true
  end

  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    local v = transition.v
    if not color[v] then
      index = visit1(v, not_dead_states, color)
    end
    if not_dead_states[v] then
      not_dead_states[u] = true
    end
  end

  color[u] = 2
end

local function visit2(u, not_dead_states, color)
  color[u] = 1

  local transitions = u.transitions
  local new_transitions = {}
  for i = 1, #transitions do
    local transition = transitions[i]
    local v = transition.v
    if not color[v] then
      index = visit2(v, not_dead_states, color)
    end
    if not_dead_states[v] then
      new_transitions[#new_transitions + 1] = transition
    end
  end
  u.transitions = new_transitions

  color[u] = 2
end

function module.remove_dead_states(u)
  local not_dead_states = {}
  visit1(u, not_dead_states, {})
  visit2(u, not_dead_states, {})
  return u
end

---------------------------------------------------------------------------

local function visit(u, states, indices, index, color)
  color[u] = 1
  index = index + 1
  indices[u] = index
  states[index] = u

  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    local v = transition.v
    if not color[v] then
      index = visit(v, states, indices, index, color)
    end
  end

  color[u] = 2
  return index
end

local function create_states_and_indices(u)
  local states = {}
  local indices = {}
  visit(u, states, indices, 0, {})
  return states, indices
end

local function execute_transition(u, byte)
  if u then
    local transition = u:execute_transition(byte)
    if transition then
      return transition, transition.v, transition.action
    end
  end
end

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
  local x_states, x_indices = create_states_and_indices(ux)
  local y_states, y_indices = create_states_and_indices(uy)

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
        local actions = {}
        local new_transition_map = {}

        for byte = 0x00, 0xFF do
          local tx, vx, action = execute_transition(ux, byte)
          local ty, vy = execute_transition(uy, byte)

          local vkey = 0
          local timestamp
          if ty then
            vkey = y_indices[vy] * n
            timestamp = ty.timestamp
          end
          if tx then
            vkey = x_indices[vx] + vkey
            timestamp = tx.timestamp
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

            local new_transition_key = module.new_transition_key(vkey, actions, action)
            local new_transition = new_transition_map[new_transition_key]
            if not new_transition then
              new_transition = unew:transition(vnew, { [byte] = true })
              new_transition.action = action
              new_transition.timestamp = timestamp
              new_transition_map[new_transition_key] = new_transition
            else
              new_transition.set[byte] = true
              if new_transition.timestamp > timestamp then
                new_transition.timestamp = timestamp
              end
            end
          end
        end
      end
    end
  end

  local unew = new_states[x_indices[ux] + y_indices[uy] * n]
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
