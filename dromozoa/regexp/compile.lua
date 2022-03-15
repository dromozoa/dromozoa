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
      out:write(name, "=", code, "\n")
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

return function(out, data)
out:write([[
local _ = {}
]])

  local compactor = {}
  local compactor_index = 0

  for i = 1, #data do
    local def = data[i]
    local name = def.name
    local code

    code, compactor_index = dump_transitions(out, def.transitions, compactor, compactor_index)

    out:write(([[
local %s = %d
local %s_transitions = %s
]]):format(name, i, name, code))


  end
end
