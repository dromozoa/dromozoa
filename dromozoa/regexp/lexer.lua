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

local union = require "dromozoa.regexp.union"

return function (data)
  local definitions = {}

  for k, v in pairs(data) do
    local name
    if type(k) == "string" then
      name = k
    else
      name = assert(v.literal)
    end
    definitions[#definitions + 1] = { timestamp = assert(v.timestamp), name = name, def = v }
  end

  table.sort(definitions, function (a, b) return a.timestamp < b.timestamp end)

  local token_names = {}
  local token_codes = {}

  for i = 1, #definitions do
    local name = definitions[i].name
    token_names[i] = name
    token_codes[name] = i

    -- local accept = "token=" .. i
    -- local def = definitions[i].def % 

    local u, v = tree_to_nfa(definitions[i].def, "push_token()")
    v.accept_token = i
  end


end
