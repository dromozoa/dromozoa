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

return function (symbol_names, that)
  local data = {}
  for head, body in pairs(that) do
    local timestamp = body[1].timestamp
    data[#data + 1] = {
      timestamp = timestamp;
      item = { head = head, body = body };
    }
  end
  table.sort(data, function (a, b) return a.timestamp < b.timestamp end)

  for i = 1, #data do
    data[i] = data[i].item
  end
  return data
end
