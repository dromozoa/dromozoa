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

local list = require "dromozoa.list"
local tree_set = require "dromozoa.tree_set"
local runtime = require "dromozoa.regexp.runtime"

local function make_shared(shared_map, shared_data, v)
  local _, i, ok = shared_map:insert(v)
  if ok then
    shared_data:append("{" .. table.concat(v, ",") .. "};\n")
    assert(i == #shared_data)
  end
  return i
end

local function make_action(action_data, action_threads, v)
  local _, i, ok = action_data:insert("function()" .. v .. "\nend;\n")
  -- 最後にやったほうがよいかもしれない
  if ok then
    -- コルーチンの必要性をおおまかに検査する。
    -- 1. 単語境界を調べやすくするために番兵を置く。
    local s = " " .. v .. " "
    -- 2. fcallという単語が最初に出現する位置を調べる。
    local p = s:find "[^%w_](fcall)[^%w_]"
    -- 3. fcallという単語が最後に出現する位置を調べる。
    local q = s:find "[^%w_](fcall)%s*%b()%s*$"
    if p == q then
      action_threads:append(0)
    else
      action_threads:append(1)
    end
    assert(i == #action_threads)
  end
  return i

end

local function update_state_indices_accept(u, action_data, action_threads, accept_actions, color)
  color[u] = 1
  if u.accept_action ~= nil then
    u.index = #accept_actions:append(make_action(action_data, action_threads, u.accept_action))
  end
  for _, t in ipairs(u.transitions) do
    if color[t.v] == nil then
      update_state_indices_accept(t.v, action_data, action_threads, accept_actions, color)
    end
  end
  color[u] = 2
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

local function construct_table(u, max_state, transitions, action_data, action_threads, transition_actions, transition_states, color)
  color[u] = 1
  for _, t in ipairs(u.transitions) do
    local code = t.v.index
    if t.action ~= nil then
      code = max_state + #transition_actions:append(make_action(action_data, action_threads, t.action))
      transition_states:append(t.v.index)
    end
    for byte in pairs(t.set) do
      transitions[byte][u.index] = code
    end
    if color[t.v] == nil then
      construct_table(t.v, max_state, transitions, action_data, action_threads, transition_actions, transition_states, color)
    end
  end
  color[u] = 2
end

local function generate(index, u, guard_action, shared_map, shared_data, static_data, action_data, action_threads)
  local accept_actions = list()
  update_state_indices_accept(u, action_data, action_threads, accept_actions, {})
  local max_state = update_state_indices_nonaccept(u, #accept_actions, {})

  local transitions = {}
  for byte = 0x00, 0xFF do
    transitions[byte] = {}
    for i = 1, max_state do
      transitions[byte][i] = 0
    end
  end
  local transition_actions = list()
  local transition_states = list()

  construct_table(u, max_state, transitions, action_data, action_threads, transition_actions, transition_states, {})

  static_data:append(
    "{\n",
    "start_state=", u.index, ";\n",
    "max_accept_state=", #accept_actions, ";\n",
    "max_state=", max_state, ";\n",
    "transitions={[0]=")
  for byte = 0x00, 0xFF do
    static_data:append("_[", make_shared(shared_map, shared_data, transitions[byte]), "],")
  end
  static_data:append(
    "};\n",
    "transition_actions=_[", make_shared(shared_map, shared_data, transition_actions), "];\n",
    "transition_states=_[", make_shared(shared_map, shared_data, transition_states), "];\n",
    "accept_actions=_[", make_shared(shared_map, shared_data, accept_actions), "];\n")
  if guard_action ~= nil then
    static_data:append("guard_action=", make_action(action_data, action_threads, guard_action), ";\n")
  end
  static_data:append(
    "};\n")
end

return function (that)
  local shared_map = tree_set()
  local shared_data = list()
  local static_data = list()
  local custom_data = list()
  local action_data = tree_set()
  local action_threads = list()

  local data = list()
  for k, v in pairs(that) do
    if type(k) == "string" then
      data:append { timestamp = v.timestamp, machine = v, name = k }
    end
  end
  local j = 0
  for i, v in ipairs(that) do
    if type(v) == "string" then
      custom_data:append(v, "\n")
    else
      j = j + 1
      data:append { timestamp = v.timestamp, machine = v, main = j == 1 }
    end
  end
  table.sort(data, function (a, b) return a.timestamp < b.timestamp end)

  for i, v in ipairs(data) do
    if v.main then
      static_data:append("main=", i, ";\n")
    end
    if v.name ~= nil then
      custom_data:append("local ", v.name, "=", i, "\n")
    end
    generate(i, v.machine.start_state, v.machine.guard_action, shared_map, shared_data, static_data, action_data, action_threads)
  end

  static_data:append("action_threads=_[", make_shared(shared_map, shared_data, action_threads), "];\n")

  return table.concat(runtime {
    shared_data = table.concat(shared_data);
    static_data = table.concat(static_data);
    custom_data = table.concat(custom_data);
    action_data = action_data:concat();
  })
end
