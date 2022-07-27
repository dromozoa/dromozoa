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
local compare = require "dromozoa.compare"
local tree_map = require "dromozoa.tree_map"

local module = {}

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.regexp.pattern" }

local timestamp = 0

local function construct(code, ...)
  timestamp = timestamp + 1
  return setmetatable({ timestamp = timestamp, [0] = code, ... }, metatable)
end

local function pattern(that)
  if type(that) == "string" then
    local self = construct("[", { [that:byte(1)] = true })
    for i = 2, #that do
      self = self + construct("[", { [that:byte(i)] = true })
    end
    self.literal = that
    return self
  else
    return that
  end
end

local function range(that)
  if type(that) == "string" then
    local set = {}
    for i = 1, #that, 2 do
      local a, b = that:byte(i, i + 1)
      if b == nil then
        b = a
      end
      for byte = a, b do
        set[byte] = true
      end
    end
    return construct("[", set)
  else
    return pattern(that)
  end
end

local function set(that)
  if type(that) == "string" then
    local set = {}
    for i = 1, #that do
      set[that:byte(i)] = true
    end
    return construct("[", set)
  else
    return pattern(that)
  end
end

local function union(self, that)
  local self = pattern(self)
  local that = pattern(that)
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

function metatable:__add(that)
  local self = pattern(self)
  local that = pattern(that)
  if self[0] == "%" or that[0] == "%" then
    error "not supported"
  else
    return construct(".", self, that)
  end
end

function metatable:__sub(that)
  local self = pattern(self)
  local that = pattern(that)
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
  local self = pattern(self)
  if self[0] == "[" then
    return construct("/", self, that)
  else
    error "not supported"
  end
end

function metatable:__mod(that)
  local self = pattern(self)
  if self[0] == "%" then
    error "not supported"
  else
    local result = construct("%", self, that)
    result.literal = self.literal
    return result
  end
end

function metatable:__unm()
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

module.pattern = setmetatable({}, {
  __index = function (_, that)
    return range(that)
  end;

  __call = function (_, that)
    if type(that) == "table" and getmetatable(that) ~= metatable then
      local result = set(that[1])
      for i = 2, #that do
        result = union(result, set(that[i]))
      end
      return result
    else
      return pattern(that)
    end
  end;
})

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.regexp.list" }

function class:append(...)
  local n = #self
  for i = 1, select("#", ...) do
    self[#self + 1] = select(i, ...)
  end
  return self
end

function module.list(...)
  return setmetatable({}, metatable):append(...)
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.regexp.transition" }

function class:update(timestamp, byte)
  self.set[byte] = byte
  if self.timestamp > timestamp then
    self.timestamp = timestamp
  end
  return self
end

function module.transition(u, v, set, timestamp, action)
  local self = setmetatable({ v = v, set = set, timestamp = timestamp, action = action }, metatable)
  u.transitions:append(self)
  return self
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.regexp.state" }

function class:simulate(byte, move)
  for _, transition in ipairs(self.transitions) do
    if transition.set ~= nil and transition.set[byte] then
      if move ~= nil and (move.timestamp == nil or move.timestamp > transition.timestamp) then
        move.timestamp = transition.timestamp
        move.action = transition.action
      end
      return transition.v, transition.action
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

function module.state()
  return setmetatable({ transitions = module.list() }, metatable)
end

---------------------------------------------------------------------------

local function node_to_nfa(node)
  local code = node[0]
  if code == "[" then
    local u = module.state()
    local v = module.state()
    module.transition(u, v, node[1], timestamp)
    return u, v
  else
    local au, av = node_to_nfa(node[1])
    if code == "/" then
      au.transitions[1].action = node[2]
      return au, av
    elseif code == "%" then
      av:update(node.timestamp, node[2])
      return au, av
    elseif code == "." then
      local bu, bv = node_to_nfa(node[2])
      module.transition(av, bu)
      return au, bv
    else
      local u = module.state()
      local v = module.state()
      if code == "*" then
        module.transition(u, v)
        module.transition(u, au)
        module.transition(av, au)
        module.transition(av, v)
      elseif code == "+" then
        module.transition(u, au)
        module.transition(av, au)
        module.transition(av, v)
      elseif code == "?" then
        module.transition(u, v)
        module.transition(u, au)
        module.transition(av, v)
      else
        local bu, bv = node_to_nfa(node[2])
        if code == "|" then
          module.transition(u, au)
          module.transition(u, bu)
          module.transition(av, v)
          module.transition(bv, v)
        elseif code == "-" then
          -- local timestamp = node.timestamp
          av:update(node.timestamp, true)
          bv:update(node.timestamp, true)

          local cu, accept_states = module.minimize(
            module.difference(
              module.minimize(module.nfa_to_dfa(au)),
              module.minimize(module.nfa_to_dfa(bu))))

          module.transition(u, cu)
          for _, cv in ipairs(accept_states) do
            cv.timestamp = nil
            cv.accept_action = nil
            module.transition(cv, v)
          end
        end
      end
      return u, v
    end
  end
end

-- TODO accept_actionのnilの扱いを検討する
-- lexer側の呼び出しに応じて考える
function module.tree_to_nfa(root, accept_action)
  local u, v = node_to_nfa(root)
  if not v.accept_action then
    v:update(root.timestamp, accept_action or true)
  end
  return u, v
end

---------------------------------------------------------------------------

local function update_state_indices(u, states, color)
  color[u] = 1
  u.index = #states:append(u)
  for _, transition in ipairs(u.transitions) do
    if color[transition.v] == nil then
      update_state_indices(transition.v, states, color)
    end
  end
  color[u] = 2
end

function module.update_state_indices(u)
  local states = module.list()
  update_state_indices(u, states, {})
  return states
end

---------------------------------------------------------------------------

local function epsilon_closure(u, closure)
  for _, transition in ipairs(u.transitions) do
    if transition.set == nil then
      closure[transition.v.index] = transition.v
      epsilon_closure(transition.v, closure)
    end
  end
end

function module.epsilon_closure(u, epsilon_closures)
  local closure = epsilon_closures[u]
  if closure == nil then
    closure = tree_map()
    closure[u.index] = u
    epsilon_closure(u, closure)
    epsilon_closures[u] = closure
  end
  return closure
end

local function new_state(closure)
  local state = module.state()
  for _, u in pairs(closure) do
    state:update(u.timestamp, u.accept_action)
  end
  return state
end

local function nfa_to_dfa(umap, unew, states, epsilon_closures, color)
  color[umap] = 1

  local new_states = module.list()
  local new_transition_map = tree_map()

  for byte = 0x00, 0xFF do
    local vmap = tree_map()
    local move = {}

    for _, u in pairs(umap) do
      local to = u:simulate(byte, move)
      if to ~= nil then
        for k, v in pairs(module.epsilon_closure(to, epsilon_closures)) do
          vmap[k] = v
        end
      end
    end

    if vmap():next() ~= nil then
      local vnew = states[vmap]
      if vnew == nil then
        vnew = new_state(vmap)
        states[vmap] = vnew
        new_states:append { map = vmap, state = vnew }
      end

      local new_transition_key = { map = vmap, action = move.action }
      local new_transition = new_transition_map[new_transition_key]
      if new_transition == nil then
        new_transition_map[new_transition_key] = module.transition(unew, vnew, { [byte] = true }, move.timestamp, move.action)
      else
        new_transition:update(move.timestamp, byte)
      end
    end
  end

  for _, item in ipairs(new_states) do
    if color[item.map] == nil then
      nfa_to_dfa(item.map, item.state, states, epsilon_closures, color)
    end
  end

  color[umap] = 2
end

function module.nfa_to_dfa(u)
  module.update_state_indices(u)
  local states = tree_map()
  local epsilon_closures = {}
  local umap = module.epsilon_closure(u, epsilon_closures)
  local unew = new_state(umap)
  states[umap] = unew
  nfa_to_dfa(umap, unew, states, epsilon_closures, tree_map())
  return unew
end

---------------------------------------------------------------------------

local assertion = true

local function create_initial_partitions(u, accept_partition_map, nonaccept_partition, partition_map, color)
  color[u] = 1

  local partition = nonaccept_partition
  if u.accept_action then
    partition = accept_partition_map[u.accept_action]
    if partition == nil then
      partition = module.list()
      partition.timestamp = u.timestamp
      accept_partition_map[u.accept_action] = partition
    elseif partition.timestamp > u.timestamp then
      partition.timestamp = u.timestamp
    end
  end
  partition:append(u)
  partition_map[u] = partition

  for _, transition in ipairs(u.transitions) do
    if color[transition.v] == nil then
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

  if next(nonaccept_partition) ~= nil then
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
      -- パーティション内の状態の組(x,y)について同じ遷移をするか調べる。同じ遷
      -- 移をするならば、ひとつのパーティションにまとめる。
      for i, x in ipairs(partition) do
        for j = 1, i - 1 do
          local y = partition[j]
          -- 全ての文字について下記の条件が満たされていれば、同じ遷移をするとみ
          -- なす。
          -- 1. 遷移先の状態が同じパーティションに含まれている。
          -- 2. 同じ遷移アクションを持つ。
          local same_partition = true
          for byte = 0x00, 0xFF do
            local x_to, x_action = x:simulate(byte)
            local y_to, y_action = y:simulate(byte)
            if partition_map[x_to] ~= partition_map[y_to] or compare(x_action, y_action) ~= 0 then
              same_partition = false
              break
            end
          end

          if same_partition then
            local new_partition = new_partition_map[x]
            if new_partition == nil then
              local new_partition = new_partition_map[y]
              new_partition:append(x)
              new_partition_map[x] = new_partition
            else
              -- パーティション内でyよりも前の状態zについて、状態の組(x,z)がひ
              -- とつのパーティションにまとめられた場合、xはすでに新パーティシ
              -- ョンに登録済みになっている。このとき、状態(y,z)もひとつのパー
              -- ティションにまとめられているはずである。
              assert(new_partition == new_partition_map[y])
            end
          end
        end

        if new_partition_map[x] == nil then
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
  local accept_states = module.list()

  for i, partition in ipairs(partitions) do
    local unew = module.state()
    for _, x in ipairs(partition) do
      unew:update(x.timestamp, x.accept_action)
    end

    states[partition] = { index = i, state = unew }
    if unew.accept_action then
      accept_states:append(unew)
    end
  end

  for i, partition in ipairs(partitions) do
    local unew = states[partition].state

    local new_transition_map = tree_map()

    for byte = 0x00, 0xFF do
      local move = {}
      local to = partition[1]:simulate(byte, move)
      if to ~= nil then
        local v = states[partition_map[to]]
        for j = 2, #partition do
          -- 遷移先が同じであることを確認する。
          assert(partition[j]:simulate(byte, move) == to)
        end

        local new_transition_key = { index = v.index, action = move.action }
        local new_transition = new_transition_map[new_transition_key]
        if not new_transition then
          new_transition_map[new_transition_key] = module.transition(unew, v.state, { [byte] = true }, move.timestamp, move.action)
        else
          new_transition:update(move.timestamp, byte)
        end
      end
    end
  end

  local unew = states[partition_map[u]].state
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
    if not uy or not uy.accept_action then
      state:update(ux.timestamp, ux.accept_action)
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
          local move = {}
          local vx, action
          if ux then
            vx, action = ux:simulate(byte, move)
          end
          local vy
          if uy then
            -- timestampはvxのuxのものであるべき（ただし、uxが存在しないか、遷移しない場合がある）
            if vx then
              vy = uy:simulate(byte)
            else
              vy = uy:simulate(byte, move)
            end
          end

          local vkey = 0
          if vy then
            vkey = vy.index * n
          end
          if vx then
            vkey = vx.index + vkey
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
              new_transition_map[new_transition_key] = module.transition(unew, vnew, { [byte] = true }, move.timestamp, action)
            else
              new_transition:update(move.timestamp, byte)
            end
          end
        end
      end
    end
  end

  local unew = new_states[ux.index + uy.index * n]
  return module.remove_dead_states(unew)
end

---------------------------------------------------------------------------

local _ = module.pattern

local x = _{ _"a"{0} + _"b"{1} + (_"c"/"T"){0,1} - "abc" ; _["xyz"]{3,3} } %"A"
local d = module.nfa_to_dfa(module.tree_to_nfa(x, true))

local out = assert(io.open("test.dot", "w"))
write_graphviz(out, d)
out:close()
