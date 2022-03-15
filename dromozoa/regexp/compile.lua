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

local function dump_transitions(out, data, compactor, compactor_index)
  local buffer = {}
  for byte = 0x00, 0xFF do
    local code = "{" .. table.concat(data[byte], ",") .. "}"
    local name = compactor[code]
    if not name then
      compactor_index = compactor_index + 1
      name = "_[" .. compactor_index .. "]"
      out:write("  ", code, ";\n")
      compactor[code] = name
    end
    if byte == 0 then
      buffer[#buffer + 1] =  "[0]=" .. name
    else
      buffer[#buffer + 1] =  name
    end
  end
  return "{" .. table.concat(buffer, ",") .. "}", compactor_index
end

local function dump_actions(out, data)
  for i = 1, #data do
    local action = data[i]
    out:write "        "
    if type(action) == "string" then
      out:write("function () ", action, " end")
    else
      out:write "function () end"
    end
    out:write ";\n"
  end
end

return function(out, data)
  out:write [[
local _ = {
]]

  local compactor = {}
  local compactor_index = 0
  local transitions = {}

  for i = 1, #data do
    transitions[i], compactor_index = dump_transitions(out, data[i].transitions, compactor, compactor_index)
  end

  out:write [[
}

local T = {
]]

  for i = 1, #data do
    local item = data[i]
    out:write(([[
  {
    name = "%s";
    guard = %s;
    max_accept_state = %d;
    transition_to_states = {%s};
    start_state = %d;
    max_state = %d;
    transitions = %s;
  };
]]):format(
      item.name,
      item.guard and "true" or "false",
      item.max_accept_state,
      table.concat(item.transition_to_states, ","),
      item.start_state,
      item.max_state,
      transitions[i]))
  end

  out:write [[
}

]]

  for i = 1, #data do
    out:write(([[
local %s = %d
]]):format(data[i].name, i))
  end

  out:write [[

return function (s)
  local fc
  local fp

  local function fcall(name)
  end

  local function fgoto(name)
  end

  local function fret()
  end

  local A = {
]]

  for i = 1, #data do
    local item = data[i]

    out:write [[
    {
      accept_actions = {
]]

    dump_actions(out, item.accept_actions)

    out:write [[
      };
      transition_actions = {
]]

    dump_actions(out, item.transition_actions)

    out:write [[
      };
    };
]]
  end

  out:write [[
  }
end
]]
end
