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


  -- item.start_state = u.index
  -- item.accept_actions = accept_actions
  -- item.max_state = max_state
  -- item.transitions = transitions
  -- item.transition_actions = transition_actions
  -- item.transition_states = transition_states
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

  return table.concat(runtime {
    shared_data = table.concat(shared_data);
    static_data = table.concat(static_data);
    action_data = table.concat(action_data);
  })
end

--[====[
local template1 = [[
return function (source, source_name)
  local fgoto
  local fcall
  local fret
  local clear
  local append
  local append_range
  local append_utf8
  local push_token
  local skip_token

  local fs = 1  -- start position
  local fp      -- current position
  local fc      -- current character
  local fb = {} -- string buffer
  local fg = {} -- guard buffer
  local ln = 1  -- line number
  local lp = 0  -- line position
  local ra
  local rb
  local rc
  local rd

  local token_symbol
  local action_table = (function ()
    local hide_variables...
    return {
]]

local template2 = [[
    }
  end)()

  local start_line = 1
  local start_column = 1
  local current_position = 1
  local current_byte
  local current_index = main
  local current_state = _[current_index].start_state
  local current_loop

  local top = 0
  local stack = {}
  local tokens = {}

  fgoto = function (index)
    fs = current_position
    start_line = ln
    start_column = fs - lp
    current_index = index
    current_state = _[current_index].start_state
    current_loop = nil
  end

  fcall = function (index)
    top = top + 1
    stack[top] = {
      start_position = fs;
      start_line = start_line;
      start_column = start_column;
      current_index = current_index;
      current_state = current_state;
    }
    fgoto(index)
  end

  fret = function ()
    local item = stack[top]
    fs = item.start_position
    start_line = item.start_line
    start_column = item.start_column
    current_index = item.current_index
    current_state = item.current_state
    current_loop = nil
    stack[top] = nil
    top = top - 1
  end

  clear = function (buffer)
    buffer.n = 0
    buffer.str = nil
  end

  append = function (buffer, data)
    local n = buffer.n + 1
    if not data then
      buffer[n] = string.char(fc)
    elseif type(data) == "number" then
      buffer[n] = string.char(data)
    else
      buffer[n] = tostring(data)
    end
    buffer.n = n
    buffer.str = nil
  end

  append_range = function (buffer)
    append(buffer, string.sub(source, fs, fp))
  end;

  append_utf8 = function (buffer, a)
    if a <= 0x7F then
      append(buffer, string.char(a))
    elseif a <= 0x07FF then
      local b = a % 0x40
      local a = (a - b) / 0x40
      append(buffer, string.char(a + 0xC0, b + 0x80))
    elseif a <= 0xFFFF then
      local c = a % 0x40
      local a = (a - c) / 0x40
      local b = a % 0x40
      local a = (a - b) / 0x40
      append(buffer, string.char(a + 0xE0, b + 0x80, c + 0x80))
    else
      local d = a % 0x40
      local a = (a - d) / 0x40
      local c = a % 0x40
      local a = (a - c) / 0x40
      local b = a % 0x40
      local a = (a - b) / 0x40
      append(buffer, string.char(a + 0xF0, b + 0x80, c + 0x80, d + 0x80))
    end
  end

  push_token = function (value)
    local source = string.sub(source, fs, fp)
    if not value then
      value = source
    elseif type(value) == "table" then
      local s = value.str
      if not s then
        s = table.concat(value, "", 1, value.n)
        value.str = s
      end
      value = s
    end
    tokens[#tokens + 1] = {
      symbol = token_symbol;
      i = fs;
      j = fp;
      source = source;
      line = start_line;
      column = start_column;
      value = value;
    }
  end

  skip_token = function ()
    tokens[#tokens + 1] = {
      i = fs;
      j = fp;
      source = string.sub(source, fs, fp);
      line = start_line;
      column = start_column;
    }
  end

  while true do
    local guard_action = _[current_index].guard_action
    local guarded
    if guard_action and current_state == _[current_index].start_state then
      local guard = fg.str
      if not guard then
        guard = table.concat(fg, "", 1, fg.n)
        fg.str = guard
      end
      local p = current_position + #guard - 1
      guarded = string.sub(source, current_position, p) == guard
      if guarded then
        current_position = p + 1
        fp = p
        fc = string.byte(source, p)
        guard_action()
      end
    end

    if not guarded then
      current_byte = string.byte(source, current_position)

      local s = 0
      if current_byte then
        s = _[current_index].transitions[current_byte][current_state]
      end

      if s == 0 then
        local error_message
        if current_state <= _[current_index].max_accept_state then
          current_loop = _[current_index].loop
          _[current_index].accept_actions[current_state]()
          if not current_byte then
            if current_index == main then
              return tokens
            end
            error_message = "unexpected eof"
          end
          if current_loop then
            fgoto(current_index)
          end
        else
          error_message = "regexp error"
        end
        if error_message then
          if not source_name then
            source_name = "?"
          end
          error(source_name .. ":" .. start_line .. ":" .. start_column .. ": " .. error_message)
        end
      else
        fp = current_position
        fc = current_byte
        if s > _[current_index].max_state then
          local transition = s - _[current_index].max_state
          current_position = current_position + 1
          current_state = _[current_index].transition_to_states[transition]
          _[current_index].transition_actions[transition]()
        else
          current_position = current_position + 1
          current_state = s
        end
      end
    end
  end
end
]]

local function compact_transitions(out, transitions, compactor)
  local buffer = {}
  for byte = 0x00, 0xFF do
    local code = "{" .. table.concat(transitions[byte], ",") .. "}"
    local name = compactor[code]
    if not name then
      local index = compactor.index + 1
      name = "c[" .. index .. "]"
      out:write(code, ";\n")
      compactor.index = index
      compactor[code] = name
    end
    if byte == 0 then
      buffer[#buffer + 1] = "[0]=" .. name
    else
      buffer[#buffer + 1] = name
    end
  end
  return "{" .. table.concat(buffer, ",") .. "}"
end

local function dump_action(out, action, compactor)
  local code
  if action then
    if type(action) == "string" then
      code = "function () " .. action .. " end"
    else
      code = "function () end"
    end
  else
    code = "false"
  end
  local name = compactor[code]
  if not name then
    local index = compactor.index + 1
    name = "c[" .. index .. "]"
    out:write(code, ";\n")
    compactor.index = index
    compactor[code] = name
  end
  return name
end

local function dump_actions(out, actions, compactor)
  local buffer = {}
  for i = 1, #actions do
    buffer[#buffer + 1] = dump_action(out, actions[i], compactor)
  end
  return "{" .. table.concat(buffer, ",") .. "}"
end

return function(out, data)
  local n = #data

  local compactor = { index = 0 }
  local transitions = {}

  out:write "local c={\n"
  for i = 1, n do
    transitions[i] = compact_transitions(out, data[i].transitions, compactor)
  end
  out:write "}\n"

  out:write "local _={\n"
  for i = 1, n do
    local item = data[i]
    out:write "{\n"
    out:write("transition_to_states={", table.concat(item.transition_to_states, ","), "};\n")
    out:write("transitions=", transitions[i], ";\n")
    out:write "};\n"
  end
  out:write "}\n"

  for i = 1, n do
    out:write("local ", data[i].name, "=", i, "\n")
  end

  out:write(template1)

  local compactor = { index = 0 }
  local guard_action = {}
  local accept_actions = {}
  local transition_actions = {}

  out:write "local c={\n"
  for i = 1, n do
    local item = data[i]
    guard_action[i] = dump_action(out, item.guard_action, compactor)
    accept_actions[i] = dump_actions(out, item.accept_actions, compactor)
    transition_actions[i] = dump_actions(out, item.transition_actions, compactor)
  end
  out:write "}\n"

  out:write "local _={\n"
  for i = 1, n do
    local item = data[i]
    out:write "{\n"
    out:write("loop=", item.loop and "true" or "false", ";\n")
    out:write("guard_action=", guard_action[i], ";\n")
    out:write("max_accept_state=", item.max_accept_state, ";\n")
    out:write("accept_actions=", accept_actions[i], ";\n")
    out:write("max_transition=", item.max_transition, ";\n")
    out:write("transition_to_states=_[", i, "].transition_to_states;\n")
    out:write("start_state=", item.start_state, ";\n")
    out:write("transition_actions=", transition_actions[i], ";\n")
    out:write("max_state=", item.max_state, ";\n")
    out:write("transitions=_[", i, "].transitions;\n")
    out:write "};\n"
  end
  out:write "}\n"

  out:write(template2)
end
]====]
