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

local grammar = require "dromozoa.parser.grammar"

local dumper = require "dromozoa.commons.dumper"

local left = grammar.left
local right = grammar.right
local _ = grammar.body

local g = grammar({ "+", "-", "*", "id" }, {
  left "+";
  left "*";
  right "UNM";

  main
    = _"E"
    | _"F"
    ;

  E = _"E" "+" "E"
    | _"E" "*" "E"
    | _"-" "E" :prec "UNM" %[[action]]
    | _"id";
  F = _();
})

print(dumper.encode(g, { pretty = true, stable = true }))
