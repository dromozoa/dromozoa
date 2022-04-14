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

local construct_map_of_production_indices = require "dromozoa.parser.construct_map_of_production_indices"

local function closure(max_terminal_symbol, productions, map_of_production_indices, items)
  local added_table = {}
  local m = 1
  while true do
    local n = #items
    if m > n then
      break
    end
    for i = m, n do
      local item = items[i]
      local symbol = productions[item.id].body[item.dot]
      if symbol and symbol > max_terminal_symbol and not added_table[symbol] then
        local production_ids = map_of_production_indices[symbol]
        for j = 1, #production_ids do
          items[#items + 1] = { id = production_ids[j], dot = 1 }
        end
        added_table[symbol] = true
      end
    end
    m = n + 1
  end
end

return function (max_terminal_symbol, productions)
  local map_of_production_indices = construct_map_of_production_indices(productions)
  local start_items = { { id = 1, dot = 1 } }
  lr0_closure(max_terminal_symbol, productions, map_of_production_indices, start_items)
end
