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

local function construct_map_of_production_ids(productions)
  local map_of_production_ids = {}
  for i = 1, #productions do
    local production = productions[i]
    local head = production.head
    local production_ids = map_of_production_ids[head]
    if production_ids then
      production_ids[#production_ids + 1] = i
    else
      map_of_production_ids[head] = { i }
    end
  end
  return map_of_production_ids
end

return function (symbol_names, max_terminal_symbol, productions)
  local map_of_production_ids = construct_map_of_production_ids(productions)



end
