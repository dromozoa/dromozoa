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
    self.name = that
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
    result.name = self.name
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
local metatable = { __index = class, __name = "dromozoa.regexp.state" }

function class:simulate(byte, resolved)
  for _, t in ipairs(self.transitions) do
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
  return setmetatable({ transitions = module.list() }, metatable)
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

local function transition(u, v, set, timestamp, action)
  local self = setmetatable({ v = v, set = set, timestamp = timestamp, action = action }, metatable)
  u.transitions:append(self)
  return self
end

---------------------------------------------------------------------------

local difference

local function node_to_nfa(node)
  local code = node[0]
  if code == "[" then
    local u = state()
    local v = state()
    transition(u, v, node[1], timestamp)
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

local function tree_to_nfa(node, accept_action)
  local u, v = node_to_nfa(node)
  if v.accept_action == nil then
    v:update(node.timestamp, accept_action)
  end
  return u, v
end

---------------------------------------------------------------------------

local function update_state_indices_accept(u, states, color)
  color[u] = 1
  if u.accept_action ~= nil then
    u.index = #states:append(u)
  end
  for _, t in ipairs(u.transitions) do
    if color[t.v] == nil then
      update_state_indices_accept(t.v, states, color)
    end
  end
  color[u] = 2
end

local function update_state_indices_nonaccept(u, states, color)
  color[u] = 1
  if u.accept_action == nil then
    u.index = #states:append(u)
  end
  for _, t in ipairs(u.transitions) do
    if color[t.v] == nil then
      update_state_indices_nonaccept(t.v, states, color)
    end
  end
  color[u] = 2
end

local function update_state_indices(u)
  local states = module.list()
  update_state_indices_accept(u, states, {})
  local max_accept_state = #states
  update_state_indices_nonaccept(u, states, {})
  return states, max_accept_state
end

---------------------------------------------------------------------------

local function epsilon_closure_impl(u, closure)
  for _, t in ipairs(u.transitions) do
    if t.set == nil then
      closure[t.v.index] = t.v
      epsilon_closure_impl(t.v, closure)
    end
  end
end

local function epsilon_closure(u, epsilon_closures)
  local closure = epsilon_closures[u]
  if closure == nil then
    closure = tree_map()
    closure[u.index] = u
    epsilon_closure_impl(u, closure)
    epsilon_closures[u] = closure
  end
  return closure
end

local function closure_to_state(closure, states)
  local u = states[closure]
  if u == nil then
    u = state()
    for _, v in pairs(closure) do
      u:update(v.timestamp, v.accept_action)
    end
    states[closure] = u
  end
  return u
end

local function nfa_to_dfa_impl(u_closure, u, epsilon_closures, states, color)
  color[u_closure] = 1

  local state_map = tree_map()
  local transition_map = tree_map()

  for byte = 0x00, 0xFF do
    local resolved = {}
    local v_closure = tree_map()
    for _, u in pairs(u_closure) do
      local to = u:simulate(byte, resolved)
      if to ~= nil then
        for k, v in pairs(epsilon_closure(to, epsilon_closures)) do
          v_closure[k] = v
        end
      end
    end

    if v_closure():next() ~= nil then
      local v = closure_to_state(v_closure, states)
      state_map[v_closure] = v

      local key = { closure = v_closure, action = resolved.action }
      local t = transition_map[key]
      if t == nil then
        transition_map[key] = transition(u, v, { [byte] = true }, resolved.timestamp, resolved.action)
      else
        t:update(resolved.timestamp, byte)
      end
    end
  end

  for v_closure, v in pairs(state_map) do
    if color[v_closure] == nil then
      nfa_to_dfa_impl(v_closure, v, epsilon_closures, states, color)
    end
  end

  color[u_closure] = 2
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

  for _, t in ipairs(u.transitions) do
    if color[t.v] == nil then
      create_initial_partitions(t.v, accept_partition_map, nonaccept_partition, partition_map, color)
    end
  end

  color[u] = 2
end

local function minimize(u)
  local accept_partition_map = tree_map()
  local partition = module.list()
  local partition_map = {}
  create_initial_partitions(u, accept_partition_map, partition, partition_map, {})

  local partitions = module.list()
  for _, partition in pairs(accept_partition_map) do
    partition.index = #partitions:append(partition)
  end
  if next(partition) ~= nil then
    partition.index = #partitions:append(partition)
  end

  while true do
    local new_partition_map = {}
    local new_partitions = module.list()

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
          local new_partition = module.list(x)
          new_partition.index = #new_partitions:append(new_partition)
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
    local u = state()
    for _, x in ipairs(partition) do
      u:update(x.timestamp, x.accept_action)
    end
    states[partition] = u
    if u.accept_action ~= nil then
      accept_states:append(u)
    end
  end

  for i, partition in ipairs(partitions) do
    local u = states[partition]
    local transition_map = tree_map()

    for byte = 0x00, 0xFF do
      local resolved = {}
      local x_to, _, x_action = partition[1]:simulate(byte, resolved)
      if x_to ~= nil then
        local p = partition_map[x_to]

        for j = 2, #partition do
          local y_to, _, y_action = partition[j]:simulate(byte, resolved)
          -- 同じ遷移をすることを確認する。
          assert(p == partition_map[y_to])
          assert(compare(x_action, y_action) == 0)
        end

        local v = states[p]
        local key = { index = p.index, action = resolved.action }
        local t = transition_map[key]
        if t == nil then
          transition_map[key] = transition(u, v, { [byte] = true }, resolved.timestamp, resolved.action)
        else
          t:update(resolved.timestamp, byte)
        end
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
    local new_transitions = module.list()
    for _, t in ipairs(v.transitions) do
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
  x_states[0] = null
  y_states[0] = null

  local x_n = #x_states
  local y_n = #y_states
  local n = y_n + 1

  local z_states = {}
  for i = 0, x_n do
    local x = x_states[i]
    for j = i == 0 and 1 or 0, y_n do
      local y = y_states[j]
      local z = state()
      if y.accept_action == nil then
        z:update(x.timestamp, x.accept_action)
      end
      z_states[i * n + j] = z
    end
  end

  for i = 0, x_n do
    local x_u = x_states[i]

    for j = i == 0 and 1 or 0, y_n do
      local y_u = y_states[j]
      local z_u = z_states[i * n + j]

      local transition_map = tree_map()
      for byte = 0x00, 0xFF do
        local x_v, timestamp, action = simulate(x_u, byte, nil, null)
        local y_v, timestamp = simulate(y_u, byte, timestamp, null)
        local index = x_v.index * n + y_v.index
        if index ~= 0 then
          local z_v = z_states[index]
          local key = { index = index, action = action }
          local t = transition_map[key]
          if t == nil then
            transition_map[key] = transition(z_u, z_v, { [byte] = true }, timestamp, action)
          else
            t:update(timestamp, byte)
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

function module.union(that)
  table.sort(that, function (a, b) return a.timestamp < b.timestamp end)
  local u = state()
  for _, node in ipairs(that) do
    local v = tree_to_nfa(node, "")
    transition(u, v)
  end
  return {
    timestamp = that[1].timestamp;
    start_state = minimize(nfa_to_dfa(u));
  }
end

function module.guard(guard_action, that)
  local machine = module.union(that)
  machine.loop = true
  machine.guard_action = guard_action
  return machine
end

function module.lexer(tokens, that)
  local data = module.list()
  for name, node in pairs(that) do
    if type(name) == "string" then
      node.name = name
    end
    data:append(node)
  end
  table.sort(data, function (a, b) return a.timestamp < b.timestamp end)

  local u = state()
  for _, node in ipairs(data) do
    local v, x = tree_to_nfa(node, "")
    transition(u, v)
    if node.name ~= nil then
      local symbol = tokens[node.name]
      if symbol == nil then
        symbol = #tokens:append(node.name)
        tokens[node.name] = symbol
      end
      x.accept_action = "token_symbol=" .. symbol .. ";" .. x.accept_action .. ";push_token()"
    else
      x.accept_action = x.accept_action .. ";skip_token()"
    end
  end

  return {
    timestamp = data[1].timestamp;
    loop = true;
    start_state = minimize(nfa_to_dfa(u));
  }
end

---------------------------------------------------------------------------

local _ = module.pattern

---------------------------------------------------------------------------

local x = _{ _"a"{0} + _"b"{1} + (_"c"/"T"){0,1} - "abc" ; _["xyz"]{3,3} } %"A"
local u, accept_states = minimize(nfa_to_dfa(tree_to_nfa(x, "")))
local states, max_accept_state = update_state_indices(u)

assert(max_accept_state == 3)
assert(u.index == 4)

local out = assert(io.open("test.dot", "w"))
write_graphviz(out, u)
out:close()

---------------------------------------------------------------------------

local tokens = module.list()

local m1 = module.union {
  _"aaa" %"a";
  _"aba" %"b";
  _{"ab"}{3,3} %"b";
}

local out = assert(io.open("test-m1.dot", "w"))
write_graphviz(out, m1.start_state)
out:close()

local m2 = module.lexer(tokens, {
  _"if";
  _"then";
  _"else";
  _"elseif";
  _"end";
  integer = (_["09"]/"i"){1};
  string = _"\"" + (-_["\""]/"c"){0} + "\"";
  _{" \t\r\n"}{1};
})

local out = assert(io.open("test-m2.dot", "w"))
write_graphviz(out, m2.start_state)
out:close()
