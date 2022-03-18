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
return function (s)
  local fcall
  local fret
]]

local template2 = [[
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

local function dump_action(data)
  if data then
    if type(data) == "string" then
      return "function () " .. data .. " end"
    else
      return "function () end"
    end
  else
    return "nil"
  end
end

local function dump_actions(out, data)
  for i = 1, #data do
    out:write(dump_action(data[i]), ";\n")
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
