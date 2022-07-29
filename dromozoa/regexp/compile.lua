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
    local code
    if t.action ~= nil then
      transition_actions:append(t.action)
      transition_states:append(t.v.index)
      code = max_state + #transition_actions
    else
      code = t.v.index
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

local function generate(item, shared_map, shared_data, static_data, action_data)
  local u = item.machine.u

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

  -- TODO なんかいいかんじにする
  static_data:append(
    "{\n",
    "start_state=", u.index, ";\n",
    "max_accept_state=", #accept_actions, ";\n",
    "max_state=", max_state, ";\n",
    "transitions={[0]=")
  for byte = 0x00, 0xFF do
    static_data:append("_[", make_shared(shared_map, shared_data, transitions[byte]), byte == 0xFF and "]" or "],")
  end
  static_data:append(
    "};\n",
    "transition_states=_[", make_shared(shared_map, shared_data, transition_states), "];\n",
    "};\n")

  action_data:append "{\n"
  if item.machine.guard_action ~= nil then
    action_data:append(
      "guard_action = function () ;",
      item.machine.guard_action, "\n",
      "end;\n")
  end
  action_data:append(
    "accept_actions={\n")
  for _, accept_action in ipairs(accept_actions) do
    action_data:append(
      "function () ;",
      accept_action, "\n",
      "end;\n")
  end
  action_data:append(
    "};\n")

  action_data:append(
    "transition_actions={\n")
  for _, transition_action in ipairs(transition_actions) do
    action_data:append(
      "function () ;",
      transition_action, "\n",
      "end;\n")
  end

  action_data:append(
    "};\n")

  action_data:append "};\n"
end

return function (that)
  that[1].main = true

  local data = list()
  for name, machine in pairs(that) do
    local main = name == 1
    if type(name) ~= "string" then
      name = machine.name
    end
    data:append { timestamp = machine.timestamp, machine = machine, name = name, main = main }
  end
  table.sort(data, function (a, b) return a.timestamp < b.timestamp end)

  local shared_map = tree_map()
  local shared_data = list()
  local static_data = list()
  local action_data = list()

  for i, item in ipairs(data) do
    if item.main then
      static_data:append("main=", i, ";\n")
    end
    generate(item, shared_map, shared_data, static_data, action_data)
  end
  static_data:append "machines={\n"
  for i, item in ipairs(data) do
    if item.name ~= nil then -- should be string
      static_data:append("[", ("%q"):format(item.name), "]=", i, ";\n")
    end
  end
  static_data:append "};\n"

  return table.concat(runtime {
    shared_data = table.concat(shared_data);
    static_data = table.concat(static_data);
    action_data = table.concat(action_data);
  })
end
