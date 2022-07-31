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

local list = require "dromozoa.list"
local grammar = require "dromozoa.parser.grammar"
local generate = require "dromozoa.parser.generate"

local _ = grammar.body
local left = grammar.left

-- TODO 整理する
-- compile(parser(grammar()))
-- compile(regexp(grammar()))
--
-- compile(machine(pattern()))
--

--[=[

  _{}

grammar 
parser

grammar(tokens, {
  left "+";
  left "+";

  block
    = _
    + _"retstat"
    + _"statlist"
    + _"statlist" "retstat"
    ;

  statlist
    = _"stat"
    + _"statlist"
    + _"statlist" "stat" %[[($1, $2)]]

]=]


local G = {
  -- P.269
  grammar({ "c", "d" }, {
    S = _"C" "C";
    C = _"c" "C"
      + _"d";
  });

  -- P.281
  grammar({ "id", "+", "*", "(", ")" }, {
    left "+";
    left "*";

    E = _"E" "+" "E"
      + _"E" "*" "E"
      + _"(" "E" ")"
      + _"id";
  });

  -- P.282
  grammar({ "i", "e", "a" }, {
    S = _"i" "S" "e" "S"
      + _"i" "S"
      + _"a";
  })
}

local buffer = list()

for _, g in ipairs(G) do
  buffer:append(("-"):rep(75), "\n")
  g.first_table = generate.first_table(generate.eliminate_left_recursion(g))
  local set_of_items, transitions = generate.lalr1_items(g)
  local t = generate.lr1_construct_table(g, set_of_items, transitions, function (...)
    buffer:append(...):append "\n"
  end)

  buffer:append "|    |"
  for i = 1, t.max_nonterminal_symbol do
    buffer:append(("  %-2s |"):format(t.symbol_names[i]))
  end
  buffer:append "\n"

  for i, data in ipairs(t.actions) do
    buffer:append(("| %2d |"):format(i))
    for j = 1, t.max_nonterminal_symbol do
      local v = data[j]
      if not v then
        buffer:append "     |"
      else
        buffer:append " "
        if v <= t.max_state then
          buffer:append(("%3s"):format("s" .. v))
        else
          local v = v - t.max_state
          if v == 1 then
            buffer:append "acc"
          else
            buffer:append(("%3s"):format("r" .. v))
          end
        end
        buffer:append " |"
      end
    end
    buffer:append "\n"
  end

end

-- print(table.concat(buffer))
assert(table.concat(buffer) == [[
---------------------------------------------------------------------------
|    |  c  |  d  |  $  |  S' |  S  |  C  |
|  1 |  s2 |  s3 |     |     |  s4 |  s5 |
|  2 |  s2 |  s3 |     |     |     |  s6 |
|  3 |  r4 |  r4 |  r4 |     |     |     |
|  4 |     |     | acc |     |     |     |
|  5 |  s2 |  s3 |     |     |     |  s7 |
|  6 |  r3 |  r3 |  r3 |     |     |     |
|  7 |     |     |  r2 |     |     |     |
---------------------------------------------------------------------------
shift(6) / reduce(2) conflict resolved as reduce: precedence 1 == 1 associativity left at state(9) symbol(+)
shift(7) / reduce(2) conflict resolved as shift: precedence 2 > 1 at state(9) symbol(*)
shift(6) / reduce(3) conflict resolved as reduce: precedence 1 < 2 at state(10) symbol(+)
shift(7) / reduce(3) conflict resolved as reduce: precedence 2 == 2 associativity left at state(10) symbol(*)
|    |  id |  +  |  *  |  (  |  )  |  $  |  E' |  E  |
|  1 |  s2 |     |     |  s3 |     |     |     |  s4 |
|  2 |     |  r5 |  r5 |     |  r5 |  r5 |     |     |
|  3 |  s2 |     |     |  s3 |     |     |     |  s5 |
|  4 |     |  s6 |  s7 |     |     | acc |     |     |
|  5 |     |  s6 |  s7 |     |  s8 |     |     |     |
|  6 |  s2 |     |     |  s3 |     |     |     |  s9 |
|  7 |  s2 |     |     |  s3 |     |     |     | s10 |
|  8 |     |  r4 |  r4 |     |  r4 |  r4 |     |     |
|  9 |     |  r2 |  s7 |     |  r2 |  r2 |     |     |
| 10 |     |  r3 |  r3 |     |  r3 |  r3 |     |     |
---------------------------------------------------------------------------
shift(6) / reduce(3) conflict resolved as shift at state(5) symbol(e)
|    |  i  |  e  |  a  |  $  |  S' |  S  |
|  1 |  s2 |     |  s3 |     |     |  s4 |
|  2 |  s2 |     |  s3 |     |     |  s5 |
|  3 |     |  r4 |     |  r4 |     |     |
|  4 |     |     |     | acc |     |     |
|  5 |     |  s6 |     |  r3 |     |     |
|  6 |  s2 |     |  s3 |     |     |  s7 |
|  7 |     |  r2 |     |  r2 |     |     |
]])
