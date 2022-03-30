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
local grammar = require "dromozoa.parser.grammar"
local eliminate_left_recursions = require "dromozoa.parser.eliminate_left_recursions"

local json = require "dromozoa.commons.json"

local _ = body

local symbol_names = { "+", "*", "(", ")", "id" }
local max_terminal_symbol = #symbol_names

local g = grammar(symbol_names, {
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

print(json.encode(g, { pretty = true, stable = true }))

eliminate_left_recursions(symbol_names, max_terminal_symbol, g)
