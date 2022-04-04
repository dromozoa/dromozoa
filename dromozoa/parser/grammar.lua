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
  local max_terminal_symbol = #symbol_names

  local data = {}
  for head, bodies in pairs(that) do
    data[#data + 1] = { timestamp = bodies[1].timestamp, head = head, bodies = bodies }
  end
  table.sort(data, function (a, b) return a.timestamp < b.timestamp end)

  local start_name = data[1].head
  symbol_names[#symbol_names + 1] = start_name .. "'"

  local symbol_table = {}
  for i = 1, #symbol_names do
    symbol_table[symbol_names[i]] = i
  end

  for i = 1, #data do
    local name = data[i].head
    if symbol_table[name] then
      error("symbol " .. name .. " is already defined")
    end
    local symbol = #symbol_names + 1
    symbol_names[symbol] = name
    symbol_table[name] = symbol
  end

  local productions = {
    {
      head = max_terminal_symbol + 1;
      body = { symbol_table[start_name] };
    };
  }

  local check_table = {
    [max_terminal_symbol + 1] = true
  }

  for i = 1, #data do
    local item = data[i]
    local head = symbol_table[item.head]
    local bodies = item.bodies
    for j = 1, #bodies do
      local names = bodies[j]
      local body = {}
      for k = 1, #names do
        local name = names[k]
        local symbol = symbol_table[name]
        if not symbol then
          error("symbol " .. name .. " is not defined")
        end
        body[k] = symbol
        check_table[symbol] = true
      end
      productions[#productions + 1] = { head = head, body = body }
    end
  end

  for i = 1, #symbol_names do
    if not check_table[i] then
      error("symbol " .. symbol_names[i] .. " is not used")
    end
  end

  return productions
end
