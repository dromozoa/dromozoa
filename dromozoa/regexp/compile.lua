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
local tree_set = require "dromozoa.tree_set"
local runtime = require "dromozoa.regexp.runtime"

local function insert_action(context, action)
  local action = action
    :gsub("$([%a_][%w_]*)", function (variable)
      local result = context.action.variables[variable]
      if result == nil then
        error("variable " .. variable .. " not defined")
      end
      return result
    end)
    :gsub([[${<(..-)>}]], function (s)
      local buffer = {}
      for i, v in ipairs { s:byte(1, #s) } do
        buffer[i] = ("0x%02X"):format(v)
      end
      return table.concat(buffer, ",")
    end)

  local _, i, inserted = context.action.set:insert("function()" .. action .. "\nend;\n")

  if inserted then
    -- コルーチンの必要性をおおまかに検査する。
    -- 1. 単語境界を調べやすくするために番兵を置く。
    local s = " " .. action .. " "
    -- 2. fcallという単語が最初に出現する位置を調べる。
    local p = s:find "[^%w_](fcall)[^%w_]"
    -- 3. fcallという単語が最後に出現する位置を調べる。
    local q = s:find "[^%w_](fcall)%s*%b()%s*$"
    if p == q then
      context.action.threads:append(0)
    else
      context.action.threads:append(1)
    end
  end

  return i
end

local function insert_shared(context, shared)
  return (select(2, context.shared.set:insert(shared)))
end

local function update_state_indices_nonaccept(u, index, color)
  color[u] = 1
  if u.accept_action == nil then
    index = index + 1
    u.index = index
  end
  for _, t in u.transitions:ipairs() do
    if color[t.v] == nil then
      index = update_state_indices_nonaccept(t.v, index, color)
    end
  end
  color[u] = 2
  return index
end

local function construct_table(context, u, max_state, transitions, transition_actions, transition_states, color)
  color[u] = 1
  for _, t in u.transitions:ipairs() do
    local code = t.v.index
    if t.action ~= nil then
      code = max_state + transition_actions:append(insert_action(context, t.action)):size()
      transition_states:append(t.v.index)
    end
    for byte in pairs(t.set) do
      transitions:get(byte + 1):set(u.index, code)
    end
    if color[t.v] == nil then
      construct_table(context, t.v, max_state, transitions, transition_actions, transition_states, color)
    end
  end
  color[u] = 2
end

local function generate(context, index, machine)
  local u = machine.start_state

  local accept_actions = array()
  for _, v in machine.accept_states:ipairs() do
    v.index = accept_actions:append(insert_action(context, v.accept_action)):size()
  end
  local max_state = update_state_indices_nonaccept(u, accept_actions:size(), {})

  local transitions = array()
  for i = 1, 256 do
    transitions:append(array.fill(max_state, 0))
  end
  local transition_actions = array()
  local transition_states = array()
  construct_table(context, u, max_state, transitions, transition_actions, transition_states, {})

  context.static.out:append(
    "{\n",
    "start_state=", u.index, ";\n",
    "max_accept_state=", accept_actions:size(), ";\n",
    "max_state=", max_state, ";\n",
    "transitions={[0]=")
  for _, v in transitions:ipairs() do
    context.static.out:append("_[", insert_shared(context, v), "],")
  end
  context.static.out:append(
    "};\n",
    "transition_actions=_[", insert_shared(context, transition_actions), "];\n",
    "transition_states=_[", insert_shared(context, transition_states), "];\n",
    "accept_actions=_[", insert_shared(context, accept_actions), "];\n")
  if machine.guard_action ~= nil then
    context.static.out:append("guard_action=", insert_action(context, machine.guard_action), ";\n")
  end
  context.static.out:append(
    "};\n")
end

return function (that)
  local context = {
    custom = { out = array() };
    action = { set = tree_set(), variables = {}, threads = array() };
    shared = { set = tree_set(), out = array() };
    static = { out = array() };
  }

  local data = array()
  for k, v in pairs(that) do
    if type(k) == "string" then
      data:append { timestamp = v.timestamp, machine = v, name = k }
    end
  end
  local j = 0
  for i, v in ipairs(that) do
    if type(v) == "string" then
      context.custom.out:append(v, "\n")
    else
      j = j + 1
      data:append { timestamp = v.timestamp, machine = v, main = j == 1 }
    end
  end
  data:sort(function (a, b) return a.timestamp < b.timestamp end)

  for i, v in data:ipairs() do
    if v.main then
      context.static.out:append("main=", i, ";\n")
    end
    if v.name ~= nil then
      context.action.variables[v.name] = i
    end
  end

  for i, v in data:ipairs() do
    generate(context, i, v.machine)
  end
  context.static.out:append("action_threads=_[", insert_shared(context, context.action.threads), "];\n")

  for _, v in context.shared.set:ipairs() do
    context.shared.out:append("{", v:concat ",", "};\n")
  end

  return table.concat(runtime {
    custom_data = context.custom.out:concat();
    action_data = context.action.set:concat();
    shared_data = context.shared.out:concat();
    static_data = context.static.out:concat();
  })
end
