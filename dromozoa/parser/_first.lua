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

local first_symbols

--[====[
  if symbol <= self.max_terminal_symbol then
    return { [symbol] = true }
  else
    local first_table = self.first_table
    if first_table then
      return first_table[symbol]
    else
      local productions = self.productions
      local production_ids = self.map_of_production_ids[symbol]
      local first = {}
      for i = 1, #production_ids do
        local body = productions[production_ids[i]].body
        if body[1] then
          for symbol in pairs(self:first_symbols(body)) do
            first[symbol] = true
          end
        else
          first[0] = true -- epsilon
        end
      end
      return first
    end
  end
]====]

local function first_symbol(max_terminal_symbol, productions, map_of_production_indices, symbol)
  if symbol <= max_terminal_symbol then
    return { [symbol] = true }
  else
    local production_indices = map_of_production_indices[symbol]
    local first = {}
    for i = 1, #production_indices do
      local body = productions[production_indices[i]].body
      if body[1] then
        for symbol in pairs(first_symbols(max_terminal_symbol, productions, map_of_production_indices, body)) do
          first[symbol] = true
        end
      else
        first[0] = true -- epsilon
      end
    end
    return first
  end
end

--[[
function class:first_symbols(symbols)
  local first = {}
  for i = 1, #symbols do
    local symbol = symbols[i]
    for symbol in pairs(self:first_symbol(symbol)) do
      first[symbol] = true
    end
    if first[0] then -- epsilon
      first[0] = nil
    else
      return first
    end
  end
  first[0] = true -- epsilon
  return first
end
]]

first_symbols = function (max_terminal_symbol, productions, map_of_production_indices, symbols)
  local first = {}
  for i = 1, #symbols do
    local symbol = symbols[i]
    for symbol in pairs(first_symbol(max_terminal_symbol, productions, map_of_production_indices, symbol)) do
      first[symbol] = true
    end
    if first[0] then -- epsilon
      first[0] = nil
    else
      return first
    end
  end
  first[0] = true
  return first
end

return function (max_terminal_symbol, max_nonterminal_symbol, productions)
  local map_of_production_indices = construct_map_of_production_indices(productions)
  local first_table = {}
  for symbol = max_terminal_symbol + 1, max_nonterminal_symbol do
    first_table[symbol] = first_symbol(max_terminal_symbol, productions, map_of_production_indices, symbol)
  end
  return first_table
end
