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

local function construct_map_of_production_indices(productions)
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

local function move(a1, f, e, t, a2)
  return table.move(a1, f, e, t, a2)
end

return function (symbol_names, max_terminal_symbol, productions)
  local m = max_terminal_symbol + 1
  local n = #symbol_names

  local new_symbol_names = {}
  for i = 1, n do
    new_symbol_names[i] = symbol_names[i]
  end

  local map_of_production_indices = construct_map_of_production_indices(productions)
  local map_of_productions = {}

  for i = m, n do
    local left_recursions = {}
    local no_left_recursions = {}

    local production_indices = map_of_production_indices[i]
    for j = 1, #production_indices do
      local body = productions[production_indices[j]].body
      local symbol = body[1]
      if symbol and symbol > max_terminal_symbol and symbol < i then
        local productions = map_of_productions[symbol]
        for k = 1, #productions do
          local src_body = productions[k].body
          local new_body = move(src_body, 1, #src_body, 1, {})
          move(body, 2, #body, #new_body + 1, new_body)
          if i == new_body[1] then
            left_recursions[#left_recursions + 1] = { head = i, body = new_body }
          else
            no_left_recursions[#no_left_recursions + 1] = { head = i, body = new_body }
          end
        end
      else
        if i == body[1] then
          left_recursions[#left_recursions + 1] = { head = i, body = body }
        else
          no_left_recursions[#no_left_recursions + 1] = { head = i, body = body }
        end
      end
    end

    if next(left_recursions) then
      n = n + 1
      new_symbol_names[n] = symbol_names[i] .. "'"

      local productions = {}
      for j = 1, #left_recursions do
        local src_body = left_recursions[j].body
        local new_body = move(src_body, 2, #src_body, 1, {})
        new_body[#new_body + 1] = n
        productions[#productions + 1] = { head = n, body = new_body }
      end
      productions[#productions + 1] = { head = n, body = {} }
      map_of_productions[n] = productions

      local productions = {}
      for j = 1, #no_left_recursions do
        local src_body = no_left_recursions[j].body
        local new_body = move(src_body, 1, #src_body, 1, {})
        new_body[#new_body + 1] = n
        productions[#productions + 1] = { head = i, body = new_body }
      end
      map_of_productions[i] = productions
    else
      map_of_productions[i] = no_left_recursions
    end
  end

  local new_productions = {}
  for i = m, n do
    local productions = map_of_productions[i]
    for j = 1, #productions do
      new_productions[#new_productions + 1] = productions[j]
    end
  end

  return new_symbol_names, new_productions
end
