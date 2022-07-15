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
--
-- https://github.com/aidansteele/osx-abi-macho-file-format-reference
-- https://developers.wonderpla.net/entry/2021/03/19/105503

local body = require "dromozoa.parser.body"
local eliminate_left_recursions = require "dromozoa.parser.eliminate_left_recursions"
local grammar = require "dromozoa.parser.grammar"
local lr0_items = require "dromozoa.parser.lr0_items"

local debug = tonumber(os.getenv "DROMOZOA_TEST_DEBUG")
debug = debug and debug ~= 0

local _ = body

-- P.244 Figure 4.31

local symbol_names = { "+", "*", "(", ")", "id" }
local max_terminal_symbol = #symbol_names

local productions = grammar(symbol_names, {
  E = {
    _"E" "+" "T";
    _"T";
  };
  T = {
    _"T" "*" "F";
    _"F";
  };
  F = {
    _"(" "E" ")";
    _"id";
  };
})

local buffer = {}

-- symbol_names, productions = eliminate_left_recursions(symbol_names, max_terminal_symbol, productions)
for i = 1, #productions do
  local production = productions[i]
  buffer[#buffer + 1] = symbol_names[production.head]
  buffer[#buffer + 1] = " ->"
  local body = production.body
  for j = 1, #body do
    buffer[#buffer + 1] = " "
    buffer[#buffer + 1] = symbol_names[body[j]]
  end
  buffer[#buffer + 1] = "\n"
end

if debug then
  io.write(table.concat(buffer))
end

local set_of_items, transitions = lr0_items(max_terminal_symbol, productions)

--[====[
  local symbol_names = self.symbol_names
  local productions = self.productions
  for i = 1, #set_of_items do
    local items = set_of_items[i]
    out:write("======== I", i, " ==========\n")
    for j = 1, #items do
      local item = items[j]
      local production = productions[item.id]
      local body = production.body
      local dot = item.dot
      local la = item.la
      out:write(symbol_names[production.head], " ", TO)
      for k = 1, #body do
        if k == dot then
          out:write(" ", DOT)
        end
        out:write(" ", symbol_names[body[k]])
      end
      if dot == #body + 1 then
        out:write(" ", DOT)
      end
      if la then
        out:write(", ", symbol_names[la])
      end
      out:write "\n"
    end
  end
  return out

]====]

print("!!!", #set_of_items)

if debug then
  local out = io.stdout
  for i = 1, #set_of_items do
    local items = set_of_items[i]
    out:write("========== I", i, " ==========\n")
    for j = 1, #items do
      local item = items[j]
      local production = productions[item.index]
      local body = production.body
      local dot = item.dot
      out:write(symbol_names[production.head], " →")
      for k = 1, #body do
        if k == dot then
          out:write " ・"
        end
        out:write(" ", symbol_names[body[k]])
      end
      if dot == #body + 1 then
        out:write " ・"
      end
      out:write "\n"
    end
  end
end

