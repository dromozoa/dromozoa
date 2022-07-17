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

return function (productions)
  local map_of_production_indices = {}
  for i = 1, #productions do
    local production = productions[i]
    local head = production.head
    local production_indices = map_of_production_indices[head]
    if not production_indices then
      map_of_production_indices[head] = { i }
    else
      production_indices[#production_indices + 1] = i
    end
  end
  return map_of_production_indices
end
