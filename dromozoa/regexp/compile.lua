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
local tree_map = require "dromozoa.tree_map"
local runtime = require "dromozoa.regexp.runtime"

local function update_state_indices_accept(u, accept_actions, color)
  color[u] = 1
  if u.accept_action ~= nil then
    u.index = #accept_actions:append(u.accept_action)
  end
  for _, t in ipairs(u.transitions) do
    if color[t.v] == nil then
      update_state_indices_accept(t.v, accept_actions, color)
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

local function construct_table(u, max_state, transitions, transition_actions, transition_states, color)
  color[u] = 1
  for _, t in ipairs(u.transitions) do
    local code = t.v.index
    if t.action ~= nil then
      transition_actions:append(t.action)
      transition_states:append(t.v.index)
      code = max_state + #transition_actions
    end
    for byte in pairs(t.set) do
      transitions[byte][u.index] = code
    end
    if color[t.v] == nil then
      construct_table(t.v, max_state, transitions, transition_actions, transition_states, color)
    end
  end
  color[u] = 2
end

local function make_shared(shared_map, shared_data, data)
  return shared_map(data, function ()
    return #shared_data:append("{" .. table.concat(data, ",") .. "};\n")
  end)
end

local function make_action(action_map, action_data, data)
  return action_map(data, function ()
    return #action_data:append("function()" .. data .. "\nend;\n")
  end)
end

local function generate(index, u, guard_action, shared_map, shared_data, static_data, action_map, action_data, merged_data)
  local accept_actions = list()
  update_state_indices_accept(u, accept_actions, {})
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

  construct_table(u, max_state, transitions, transition_actions, transition_states, {})

  static_data:append(
    "{\n",
    "transitions={[0]=")
  for byte = 0x00, 0xFF do
    static_data:append("_[", make_shared(shared_map, shared_data, transitions[byte]), "],")
  end
  static_data:append(
    "};\n",
    "transition_states=_[", make_shared(shared_map, shared_data, transition_states), "];\n",
    "};\n")

  merged_data:append(
    "{\n",
    "start_state=", u.index, ";\n",
    "max_accept_state=", #accept_actions, ";\n",
    "max_state=", max_state, ";\n",
    "transitions=S[", index, "].transitions;\n",
    "transition_states=S[", index, "].transition_states;\n")

  if guard_action ~= nil then
    merged_data:append("guard_action=_[", make_action(action_map, action_data, guard_action), "],\n")
  end
  merged_data:append "accept_actions={"
  for _, accept_action in ipairs(accept_actions) do
    merged_data:append("_[", make_action(action_map, action_data, accept_action), "],")
  end
  merged_data:append "};\n"
  merged_data:append "transition_actions={"
  for _, transition_action in ipairs(transition_actions) do
    merged_data:append("_[", make_action(action_map, action_data, transition_action), "],")
  end
  merged_data:append "};\n"
  merged_data:append "};\n"
end

return function (that)
  local shared_map = tree_map()
  local shared_data = list()
  local static_data = list()
  local custom_data = list()
  local action_map = tree_map()
  local action_data = list()
  local merged_data = list()

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
      merged_data:append("main=", i, ";\n")
    end
    if v.name ~= nil then
      custom_data:append("local ", v.name, "=", i, "\n")
    end
    generate(i, v.machine.start_state, v.machine.guard_action, shared_map, shared_data, static_data, action_map, action_data, merged_data)
  end

  return table.concat(runtime {
    shared_data = table.concat(shared_data);
    static_data = table.concat(static_data);
    custom_data = table.concat(custom_data);
    action_data = table.concat(action_data);
    merged_data = table.concat(merged_data);
  })
end
