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
  local assign
  local append
  local push_token
  local skip_token

  local fs      -- current start
  local fp      -- current position
  local fc      -- current character
  local fb = {} -- buffer
  local fg = {} -- guard
  local ln = 1  -- line number
  local lp = 0  -- line position
  local ra
  local rb
  local rc
  local rd

  local token_symbol
]]

local template2 = [[
  local current_position = 1
  local current_byte = 0
  local current_index = main
  local current_state = _[current_index].start_state

  local top = 0
  local stack = {}
  local buffer
  local guard

  fgoto = function (index)
    current_index = index
    current_state = _[current_index].start_state
  end

  fcall = function (index)
    top = top + 1
    stack[top] = {
      index = current_index;
      state = current_state;
    }
    fgoto(index)
  end

  fret = function ()
    local item = stack[top]
    current_index = item.index
    current_state = item.state

    stack[top] = nil
    top = top - 1
  end

  assign = function (buffer, data)
    if not data then
      buffer[1] = string.char(fc)
    elseif type(data) == "number" then
      buffer[1] = string.char(data)
    else
      buffer[1] = tostring(data)
    end
    buffer.n = 1
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
