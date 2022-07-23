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

local class = {}
local metatable = { __index = class, __name = "dromozoa.regexp.transition" }

function module.transition(u, v, set, action)
  local self = setmetatable({ v = v, set = set, action = action }, metatable)
  u.transitions:append(self)
  return self
end

---------------------------------------------------------------------------

local function node_to_nfa(node)
  local code = node[0]
  if code == "[" then
    local u = module.state()
    local v = module.state()
    module.transition(u, v, node[1]).timestamp = node.timestamp
    return u, v
  else
    local au, av = node_to_nfa(node[1])
    if code == "." then
      local bu, bv = node_to_nfa(node[2])
      module.transition(av, bu)
      return au, bv
    elseif code == "|" then
      local bu, bv = node_to_nfa(node[2])
      local u = module.state()
      local v = module.state()
      module.transition(u, au)
      module.transition(u, bu)
      module.transition(av, v)
      module.transition(bv, v)
      return u, v
    elseif code == "*" then
      local u = module.state()
      local v = module.state()
      module.transition(u, v)
      module.transition(u, au)
      module.transition(av, au)
      module.transition(av, v)
      return u, v
    elseif code == "+" then
      local u = module.state()
      local v = module.state()
      module.transition(u, au)
      module.transition(av, au)
      module.transition(av, v)
      return u, v
    elseif code == "?" then
      local u = module.state()
      local v = module.state()
      module.transition(u, v)
      module.transition(u, au)
      module.transition(av, v)
      return u, v
    elseif code == "-" then
      error "not implemented"
    elseif code == "/" then
      au.transitions[1].action = node[2]
      return au, av
    elseif code == "%" then
      av.accept_action = node[2]
      av.timestamp = node.timestamp
      return au, av
    end
  end
end

function module.tree_to_nfa(root, accept_action)
  local u, v = node_to_nfa(root)
  local timestamp = root.timestamp
  u.timestamp = timestamp
  if not v.accept_action then
    v.accept_action = accept_action
    v.timestamp = timestamp
  end
  return u, v
end

---------------------------------------------------------------------------

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
        new_transition = module.transition(unew, vnew, { [byte] = true })
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

-- TODO use __weak
local private = function () end

function module.new_transition_key(key, actions, action)
  if action then
    local index = actions[action]
    if not index then
      index = (actions[private] or 0) + 1
      actions[action] = index
      actions[private] = index
    end
    return key .. ";" .. index
  else
    return tostring(key)
  end
end

---------------------------------------------------------------------------





---------------------------------------------------------------------------

local _ = module.constructor

-- local x = _{"abc"} | _["09"]
-- local x = _"abc"{0,2}
-- local x = (_{"abc"} / "A" | "def") % "action"
-- local x = (_"a" | "b" | "c" | "z")/"transition" %"accept"
-- local x = (_["af"] + _"cz") %"action"
-- local x = _.ACac
-- local x = _"a"{0,0}
-- local x = _["ac"] | _["bd"]
-- local x = ~_["\0ad\255"]
local x
  = _"if"
  | _"then"
  | _"el" + _"s"/"1" + "e"
  | _"el" + _"s"/"2" + "eif"
  | _"end"

-- print(dumper.encode(x, { pretty = true, stable = true }))
local n = module.tree_to_nfa(x, true)
local d = module.nfa_to_dfa(n)

local out = assert(io.open("test-tree.dot", "w"))
write_graphviz_tree(out, x)
out:close()

local out = assert(io.open("test-nfa.dot", "w"))
write_graphviz(out, n)
out:close()

local out = assert(io.open("test-dfa.dot", "w"))
write_graphviz(out, d)
out:close()



