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

local tree_set = require "dromozoa.tree_set"
local runtime = require "dromozoa.regexp.runtime"

-- TODO machineと共通？
local function append(t, ...)
  local m = #t
  local n = select("#", ...)

  for i = 1, n do
    local v = select(i, ...)
    assert(v ~= nil)
    t[m + i] = v
  end

  return m + n
end

local function insert_action(context, action)
  local function substitute(variable)
    local result = context.action.variables[variable]
    if result == nil then
      error("variable " .. variable .. " not defined")
    end
    return result
  end

  local action = action
    :gsub("$([%a_][%w_]*)", substitute)
    :gsub([[${'(..-)'}]], substitute)
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
      append(context.action.threads, 0)
    else
      append(context.action.threads, 1)
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
  for _, t in ipairs(u.transitions) do
    if color[t.v] == nil then
      index = update_state_indices_nonaccept(t.v, index, color)
    end
  end
  color[u] = 2
  return index
end

local function construct_table(context, u, max_state, transitions, transition_actions, transition_states, color)
  color[u] = 1
  for _, t in ipairs(u.transitions) do
    local code = t.v.index
    if t.action ~= nil then
      code = max_state + append(transition_actions, insert_action(context, t.action))
      append(transition_states, t.v.index)
    end
    for byte in pairs(t.set) do
      transitions[byte][u.index] = code
    end
    if color[t.v] == nil then
      construct_table(context, t.v, max_state, transitions, transition_actions, transition_states, color)
    end
  end
  color[u] = 2
end

local function generate(context, index, machine)
  local u = machine.start_state

  local accept_actions = {}
  for _, v in ipairs(machine.accept_states) do
    v.index = append(accept_actions, insert_action(context, v.accept_action))
  end
  local max_state = update_state_indices_nonaccept(u, #accept_actions, {})

  local transitions = {}
  for byte = 0x00, 0xFF do
    local t = {}
    for i = 1, max_state do
      t[i] = 0
    end
    transitions[byte] = t
  end
  local transition_actions = {}
  local transition_states = {}
  construct_table(context, u, max_state, transitions, transition_actions, transition_states, {})

  append(context.static.out,
    "{\n",
    "start_state=", u.index, ";\n",
    "max_accept_state=", #accept_actions, ";\n",
    "max_state=", max_state, ";\n",
    "transitions={[0]=")
  for byte = 0x00, 0xFF do
    append(context.static.out, "_[", insert_shared(context, transitions[byte]), "],")
  end
  append(context.static.out,
    "};\n",
    "transition_actions=_[", insert_shared(context, transition_actions), "];\n",
    "transition_states=_[", insert_shared(context, transition_states), "];\n",
    "accept_actions=_[", insert_shared(context, accept_actions), "];\n")
  if machine.guard_action ~= nil then
    append(context.static.out, "guard_action=", insert_action(context, machine.guard_action), ";\n")
  end
  append(context.static.out,
    "};\n")
end

return function (that)
  local context = {
    custom = { out = {} };
    action = { set = tree_set(), variables = {}, threads = {} };
    shared = { set = tree_set(), out = {} };
    static = { out = {} };
  }

  local data = {}
  for k, v in pairs(that) do
    if type(k) == "string" then
      append(data, { timestamp = v.timestamp, machine = v, name = k })
    end
  end
  local j = 0
  for i, v in ipairs(that) do
    if type(v) == "string" then
      append(context.custom.out, v, "\n")
    else
      j = j + 1
      append(data, { timestamp = v.timestamp, machine = v, main = j == 1 })
    end
  end
  table.sort(data, function (a, b) return a.timestamp < b.timestamp end)

  for i, v in ipairs(data) do
    if v.main then
      append(context.static.out, "main=", i, ";\n")
    end
    if v.name ~= nil then
      context.action.variables[v.name] = i
    end
  end

  for i, v in ipairs(data) do
    generate(context, i, v.machine)
  end
  append(context.static.out, "action_threads=_[", insert_shared(context, context.action.threads), "];\n")

  for _, v in context.shared.set:ipairs() do
    if v.concat then
      append(context.shared.out, "{", v:concat ",", "};\n")
    else
      append(context.shared.out, "{", table.concat(v, ","), "};\n")
    end
  end

  return table.concat(runtime {
    custom_data = table.concat(context.custom.out);
    action_data = context.action.set:concat();
    shared_data = table.concat(context.shared.out);
    static_data = table.concat(context.static.out);
  })
end
