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
]]

local template2 = [[
  local current_index = main
  local current_state = _[current_index].start_state
  local current_position = 1
  local current_byte

  local stack_top = 0
  local stack = {}

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

    current_index = index
    current_state = _[current_index].start_state
  end

  fret = function ()
    local item = stack[stack_top]
    current_index = item.index
    current_state = item.state

    stack[stack_top] = nil
    stack_top = stack_top - 1
  end

  while true do
    current_byte = string.byte(source, current_position)

    local state = 0
    if current_byte then
      state = _[current_index].transitions[current_byte][current_state]
    end

    if state == 0 then
      if current_state <= _[current_index].max_accept_state then
        _[current_index].accept_actions[current_state]()
        if not current_byte then
          -- eof
          break
        end
        if current_index == main then
          -- 繰り返すのでcurrent_positionは進めない
          current_state = _[current_index].start_state
        else
          -- fgoto,fcall,fretされた場合はエラーじゃない
          -- error "regexp error"
        end
      else
        -- エラー（位置も返す）
        error "regexp error"
      end
    else
      -- 一文字読みおわった扱いにする

      local max_state = _[current_index].max_state
      if state > max_state then
        local transition = state - max_state

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
]]

local function dump_transitions(out, data, compactor, compactor_index)
  local buffer = {}
  for byte = 0x00, 0xFF do
    local code = "{" .. table.concat(data[byte], ",") .. "}"
    local name = compactor[code]
    if not name then
      compactor_index = compactor_index + 1
      name = "_[" .. compactor_index .. "]"
      out:write(code, ";\n")
      compactor[code] = name
    end
    if byte == 0 then
      buffer[#buffer + 1] = "[0]=" .. name
    else
      buffer[#buffer + 1] = name
    end
  end
  return "{" .. table.concat(buffer, ",") .. "}", compactor_index
end

local function dump_action(action)
  if action then
    if type(action) == "string" then
      return "function () " .. action .. " end"
    else
      assert(action == true)
      return "function () end"
    end
  else
    return "nil"
  end
end

local function dump_actions(out, actions)
  for i = 1, #actions do
    out:write(dump_action(actions[i]), ";\n")
  end
end

return function(out, data)
  local n = #data

  local compactor = {}
  local compactor_index = 0
  local transitions = {}

  out:write "local _={\n"
  for i = 1, n do
    transitions[i], compactor_index = dump_transitions(out, data[i].transitions, compactor, compactor_index)
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

  out:write "local _={\n"
  for i = 1, n do
    local item = data[i]
    out:write "{\n"
    out:write("guard_action=", dump_action(item.guard_action), ";\n")
    out:write("max_accept_state=", item.max_accept_state, ";\n")
    out:write "accept_actions={\n"
    dump_actions(out, item.accept_actions)
    out:write "};\n"
    out:write("max_transition=", item.max_transition, ";\n")
    out:write("transition_to_states=_[", i, "].transition_to_states;\n")
    out:write("start_state=", item.start_state, ";\n")
    out:write "transition_actions={\n"
    dump_actions(out, item.transition_actions)
    out:write "};\n"
    out:write("max_state=", item.max_state, ";\n")
    out:write("transitions=_[", i, "].transitions;\n")
    out:write "};\n"
  end
  out:write "}\n"

  out:write(template2)
end
