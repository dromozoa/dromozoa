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

local function dump_integer_array(out, data, arrays, array_index)
  local code = "{" .. table.concat(data, ",") .. "}"
  local name = arrays[code]
  if not name then
    array_index = array_index + 1
    name = "_[" .. array_index .. "]"
    arrays[code] = name
    out:write(name, "=", code, "\n")
  end
  return name, array_index
end

local function dump(dumper)
  local data = {}
  data[1] = "[0]=" .. dumper[0x00]
  for byte = 0x01, 0xFF do
    data[#data + 1] = dumper[byte]
  end
  return table.concat(data, ",")
end

return function(out, data)
out:write([[
local _ = {}
]])

  local arrays = {}
  local array_index = 0

  for i = 1, #data do
    local def = data[i]
    local name = def.name

    local transitions = def.transitions
    local dumper = {}
    for byte = 0x00, 0xFF do
      dumper[byte], array_index = dump_integer_array(out, transitions[byte], arrays, array_index)
    end

    out:write(([[
local %s = %d
local %s_transitions = {%s}
]]):format(name, i, name, dump(dumper)))


  end
end
