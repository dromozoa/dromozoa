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

local template1 = [[
return function (source)
  local fgoto
  local fcall
  local fret
  local guard_assign
  local guard_append
  local push_token
  local skip_token

  local cs = 0  -- current state
  local p  = 0  -- current position
  local c  = 0  -- current char
  local tk = 0  -- current token symbol

  local sb = {} -- string buffer
  local gb = {} -- guard buffer
  local ln = 1  -- line number
  local lp = 0  -- line position
  local ra = 0  -- general integer register
  local rb = 0  -- general integer register
  local rc = 0  -- general integer register

  local token_symbol
  local cb
  local iv
]]

local template2 = [[
  local current_index = main
  local current_state = _[current_index].start_state
  local current_position = 1
  local current_byte

  local stack_top = 0
  local stack = {}
  local buffer
  local guard

  fgoto = function (index)
    current_index = index
    current_state = _[current_index].start_state
  end

  fcall = function (index)
    stack_top = stack_top + 1
    stack[stack_top] = {
      index = current_index;
      state = current_state;
    }
    fgoto(index)
  end

  fret = function ()
    local item = stack[stack_top]
    current_index = item.index
    current_state = item.state

    stack[stack_top] = nil
    stack_top = stack_top - 1
  end

  local function char(data)
    if not data then
      return string.char(current_byte)
    elseif type(data) == "number" then
      return string.char(data)
    else
      return data
    end
  end

  guard_assign = function (data)
    guard = { char(data) }
  end

  guard_append = function (data)
    guard[#guard + 1] = char(data)
  end

  push_token = function (v)
    print("push_token", token_symbol, v)
  end

  skip_token = function ()
    print "skip_token"
  end

  while true do
    local guard_action = _[current_index].guard_action
    local guarded
    if guard_action and current_state == _[current_index].start_state then
      local guard = table.concat(guard)
      local position = current_position + #guard
      guarded = string.sub(source, current_position, position - 1) == guard
      if guarded then
        current_position = position
        guard_action()
      end
    end

    if not guarded then
      current_byte = string.byte(source, current_position)
      cb = current_byte

      local state = 0
      if current_byte then
        state = _[current_index].transitions[current_byte][current_state]
      end

      if state == 0 then
        if current_state <= _[current_index].max_accept_state then
          _[current_index].accept_actions[current_state]()
          if not current_byte then
            -- eof
            return true
          end
          if _[current_index].loop then
            current_state = _[current_index].start_state
          end
        else
          error "regexp error"
        end
      else
        if state > _[current_index].max_state then
          local transition = state - _[current_index].max_state
          current_position = current_position + 1
          current_state = _[current_index].transition_to_states[transition]
          _[current_index].transition_actions[transition]()
        else
          current_position = current_position + 1
          current_state = state
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
      assert(action == true)
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
