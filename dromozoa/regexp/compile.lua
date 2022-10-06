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

local append = require "dromozoa.append"
local runtime = require "dromozoa.regexp.runtime"

local function insert(t, v)
  assert(type(v) == "string")
  local n = t.map[v]
  if not n then
    n = append(t.set, v)
    t.map[v] = n
  end
  return n
end

local function insert_action(context, action)
  local actions = {}

  -- 単語境界を調べるために番兵を置く。
  local s = (" " .. action)
    :gsub("%$([A-Za-z_][0-9A-Za-z_]*)", context.action.variables)
    :gsub("%$%{%'(..-)%'%}", context.action.variables)
    :gsub("%$%{%<(..-)%>%}", function (a)
      return table.concat({ a:byte(1, #a) }, ",")
    end)
    -- fcall()の後の処理が存在する場合、継続として分割する。
    :gsub("(.-[^0-9A-Za-z_]fcall[\t-\r ]*%(.-%))[\t-\r ]*", function (a)
      append(actions, insert(context.action, "function()" .. a:gsub("^[\t-\r ]+", "") .. "\nend;\n"))
      return ""
    end)
  append(actions, insert(context.action, "function()" .. s:gsub("^[\t-\r ]+", "") .. "\nend;\n"))

  for i, action in ipairs(actions) do
    context.action.continuations[action] = actions[i + 1] or 0
  end
  return actions[1]
end

local function insert_shared(context, shared)
  return "_[" .. insert(context.shared, table.concat(shared, ",")) .. "]"
end

local function update_state_indices_nonaccept(u, index, color)
  color[u] = 1
  if not u.accept_action then
    index = index + 1
    u.index = index
  end
  for _, t in ipairs(u.transitions) do
    if not color[t.v] then
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
    if t.action then
      code = max_state + append(transition_actions, insert_action(context, t.action))
      append(transition_states, t.v.index)
    end
    for byte in pairs(t.set) do
      transitions[byte][u.index] = code
    end
    if not color[t.v] then
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

  local out = context.static.out
  append(out, "{\n")
  append(out, "start_state=", u.index, ";\n")
  append(out, "max_accept_state=", #accept_actions, ";\n")
  append(out, "max_state=", max_state, ";\n")
  append(out, "transitions={[0]=")
  for byte = 0x00, 0xFF do
    append(context.static.out, insert_shared(context, transitions[byte]), ",")
  end
  append(out, "};\n")
  append(out, "transition_actions=", insert_shared(context, transition_actions), ";\n")
  append(out, "transition_states=", insert_shared(context, transition_states), ";\n")
  append(out, "accept_actions=", insert_shared(context, accept_actions), ";\n")
  if machine.guard_action then
    append(out, "guard_action=", insert_action(context, machine.guard_action), ";\n")
  end
  append(out, "};\n")
end

return function (that)
  local context = {
    custom = { out = {} };
    action = { map = {}, set = {}, variables = {}, continuations = {} };
    shared = { map = {}, set = {}, out = {} };
    static = { out = {} };
  }

  local data = {}
  for k, v in pairs(that) do
    if type(k) == "string" then
      append(data, { timestamp = v.timestamp, machine = v, name = k })
    end
  end
  local main = true
  for i, v in ipairs(that) do
    if type(v) == "string" then
      append(context.custom.out, v, "\n")
    else
      append(data, { timestamp = v.timestamp, machine = v, main = main })
      main = nil
    end
  end
  table.sort(data, function (a, b) return a.timestamp < b.timestamp end)

  for i, v in ipairs(data) do
    if v.main then
      append(context.static.out, "main=", i, ";\n")
    end
    if v.name then
      context.action.variables[v.name] = i
    end
  end

  for i, v in ipairs(data) do
    generate(context, i, v.machine)
  end
  append(context.static.out, "action_continuations=", insert_shared(context, context.action.continuations), ";\n")

  for _, v in ipairs(context.shared.set) do
    append(context.shared.out, "{", v, "};\n")
  end

  return table.concat(runtime {
    custom_data = table.concat(context.custom.out);
    action_data = table.concat(context.action.set);
    shared_data = table.concat(context.shared.out);
    static_data = table.concat(context.static.out);
  })
end
