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
local first = require "dromozoa.parser.first"
local grammar = require "dromozoa.parser.grammar"

local debug = tonumber(os.getenv "DROMOZOA_TEST_DEBUG")
debug = debug and debug ~= 0

local _ = body

-- P.193 Equation 4.1
-- P.222 Example 4.30

local symbol_names = { "+", "*", "(", ")", "id" }
local max_terminal_symbol = #symbol_names

local productions = grammar(symbol_names, {
  S = {
    _"E";
  };
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

symbol_names, productions = eliminate_left_recursions(symbol_names, max_terminal_symbol, productions)
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

assert(table.concat(buffer) == [[
S' -> S
S -> E
E -> T E'
T -> F T'
F -> ( E )
F -> id
E' -> + T E'
E' ->
T' -> * F T'
T' ->
]])

local buffer = {}

local first_table = first(max_terminal_symbol, #symbol_names, productions)
for symbol = max_terminal_symbol + 1, #symbol_names do
  local first = first_table[symbol]
  buffer[#buffer + 1] = symbol_names[symbol]
  buffer[#buffer + 1] = " ="
  for symbol = 1, #symbol_names do
    if first[symbol] then
      buffer[#buffer + 1] = " "
      buffer[#buffer + 1] = symbol_names[symbol]
    end
  end
  if first[0] then
    buffer[#buffer + 1] = " \\epsilon"
  end

  buffer[#buffer + 1] = "\n"
end

if debug then
  io.write(table.concat(buffer))
end

assert(table.concat(buffer) == [[
S' = ( id
S = ( id
E = ( id
T = ( id
F = ( id
E' = + \epsilon
T' = * \epsilon
]])
