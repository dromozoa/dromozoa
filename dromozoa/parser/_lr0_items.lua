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

local function lr0_closure(max_terminal_symbol, productions, map_of_production_indices, items)
  local added_table = {}
  local m = 1
  while true do
    local n = #items
    if m > n then
      break
    end
    for i = m, n do
      local item = items[i]
      local symbol = productions[item.index].body[item.dot]
      if symbol and symbol > max_terminal_symbol and not added_table[symbol] then
        local production_indices = map_of_production_indices[symbol]
        for j = 1, #production_indices do
          items[#items + 1] = { index = production_indices[j], dot = 1 }
        end
        added_table[symbol] = true
      end
    end
    m = n + 1
  end
end

local function lr0_goto(max_terminal_symbol, productions, map_of_production_indices, items)
  local symbols = {}
  local map_of_to_items = {}
  for i = 1, #items do
    local item = items[i]
    local index = item.index
    local dot = item.dot
    local symbol = productions[index].body[dot]
    if symbol then
      local to_items = map_of_to_items[symbol]
      if to_items then
        to_items[#to_items + 1] = { index = index, dot = dot + 1 }
      else
        symbols[#symbols + 1] = symbol
        map_of_to_items[symbol] = { { index = index, dot = dot + 1 } }
      end
    end
  end
  local gotos = {}
  for i = 1, #symbols do
    local symbol = symbols[i]
    local to_items = map_of_to_items[symbol]
    lr0_closure(max_terminal_symbol, productions, map_of_production_indices, to_items)
    gotos[#gotos + 1] = {
      symbol = symbol;
      to_items = to_items;
    }
  end
  return gotos
end

--[[

  local transitions = {}
  local m = 1
  while true do
    local n = #set_of_items
    if m > n then
      break
    end
    for i = m, n do
      local transition = transitions[i]
      if not transition then
        transition = {}
        transitions[i] = transition
      end
      local gotos = self:lr0_goto(set_of_items[i])
      for j = 1, #gotos do
        local data = gotos[j]
        local to_items = data.to_items
        if to_items[1] then
          local to
          for k = 1, #set_of_items do
            if equal(to_items, set_of_items[k]) then
              to = k
              break
            end
          end
          if not to then
            to = #set_of_items + 1
            set_of_items[to] = to_items
          end
          transition[data.symbol] = to
        end
      end
    end
    m = n + 1
  end
  return set_of_items, transitions

]]

local function equal(items1, items2)
  local n = #items1
  if n ~= #items2 then
    return false
  end
  for i = 1, n do
    local item1 = items1[i]
    local item2 = items2[i]
    if item1.id ~= item2.id then
      return false
    end
    if item1.dot ~= item2.dot then
      return false
    end
    if item1.la ~= item2.la then
      return false
    end
  end
  return true
end

return function (max_terminal_symbol, productions)
  local map_of_production_indices = construct_map_of_production_indices(productions)
  local start_items = { { index = 1, dot = 1 } }
  lr0_closure(max_terminal_symbol, productions, map_of_production_indices, start_items)
  local set_of_items = { start_items }
  local transitions = {}
  local m = 1
  while true do
    local n = #set_of_items
    if m > n then
      break
    end
    for i = m, n do
      local transition = transitions[i]
      if not transition then
        transition = {}
        transitions[i] = transition
      end
      local gotos = lr0_goto(max_terminal_symbol, productions, map_of_production_indices, set_of_items[i])
      for j = 1, #gotos do
        local data = gotos[j]
        local to_items = data.to_items
        if to_items[1] then
          local to
          for k = 1, #set_of_items do
            if equal(to_items, set_of_items[k]) then
              to = k
              break
            end
          end
          if not to then
            to = #set_of_items + 1
            set_of_items[to] = to_items
          end
          transition[data.symbol] = to
        end
      end
    end
    m = n + 1
  end
  return set_of_items, transitions
end
