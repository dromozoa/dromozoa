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
local parser = require "dromozoa.parser.parser"

local _ = grammar.body
local left = grammar.left
local right = grammar.right
local nonassoc = grammar.nonassoc

-- TODO 整理する
-- compile(parser(grammar()))
-- compile(regexp(grammar()))
-- compile(machine(pattern()))

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
  });

  grammar({"id", ".."}, {
    right "..";
    E = _"E" ".." "E"
      + _"id";
  });

  grammar({"id", "=="}, {
    nonassoc "==";
    E = _"E" "==" "E"
      + _"id";
  });

  -- https://www.gnu.org/software/bison/manual/html_node/Reduce_002fReduce.html

  grammar({"id"}, {
    S = _
      + _"K"
      + _"S" "id";
    K = _
      + _"id";
  });

  grammar({"id"}, {
    S = _
      + _"S" "id"
  });
}

local buffer = list()

for _, g in ipairs(G) do
  buffer:append(("-"):rep(75), "\n")
  local t = parser(g)
  for _, message in ipairs(t.conflictions) do
    buffer:append(message, "\n")
  end

  buffer:append "|    |"
  for i = 1, t.max_nonterminal_symbol do
    buffer:append(("  %-2s |"):format(t.symbol_names[i]))
  end
  buffer:append "\n"

  for i, data in ipairs(t.actions) do
    buffer:append(("| %2d |"):format(i))
    for j = 1, t.max_nonterminal_symbol do
      local v = data[j]
      if v == 0 then
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
|  1 |  s4 |  s5 |     |     |  s2 |  s3 |
|  2 |     |     | acc |     |     |     |
|  3 |  s4 |  s5 |     |     |     |  s6 |
|  4 |  s4 |  s5 |     |     |     |  s7 |
|  5 |  r4 |  r4 |  r4 |     |     |     |
|  6 |     |     |  r2 |     |     |     |
|  7 |  r3 |  r3 |  r3 |     |     |     |
---------------------------------------------------------------------------
shift(5) / reduce(2) conflict resolved as reduce(2): precedence 1 == 1 associativity left at state(8) production(2) symbol(+)
shift(6) / reduce(2) conflict resolved as shift(6): precedence 2 > 1 at state(8) production(2) symbol(*)
shift(5) / reduce(3) conflict resolved as reduce(3): precedence 1 < 2 at state(9) production(3) symbol(+)
shift(6) / reduce(3) conflict resolved as reduce(3): precedence 2 == 2 associativity left at state(9) production(3) symbol(*)
|    |  id |  +  |  *  |  (  |  )  |  $  |  E' |  E  |
|  1 |  s4 |     |     |  s3 |     |     |     |  s2 |
|  2 |     |  s5 |  s6 |     |     | acc |     |     |
|  3 |  s4 |     |     |  s3 |     |     |     |  s7 |
|  4 |     |  r5 |  r5 |     |  r5 |  r5 |     |     |
|  5 |  s4 |     |     |  s3 |     |     |     |  s8 |
|  6 |  s4 |     |     |  s3 |     |     |     |  s9 |
|  7 |     |  s5 |  s6 |     | s10 |     |     |     |
|  8 |     |  r2 |  s6 |     |  r2 |  r2 |     |     |
|  9 |     |  r3 |  r3 |     |  r3 |  r3 |     |     |
| 10 |     |  r4 |  r4 |     |  r4 |  r4 |     |     |
---------------------------------------------------------------------------
shift(6) / reduce(3) conflict resolved as shift(6) at state(5) production(3) symbol(e)
|    |  i  |  e  |  a  |  $  |  S' |  S  |
|  1 |  s3 |     |  s4 |     |     |  s2 |
|  2 |     |     |     | acc |     |     |
|  3 |  s3 |     |  s4 |     |     |  s5 |
|  4 |     |  r4 |     |  r4 |     |     |
|  5 |     |  s6 |     |  r3 |     |     |
|  6 |  s3 |     |  s4 |     |     |  s7 |
|  7 |     |  r2 |     |  r2 |     |     |
---------------------------------------------------------------------------
shift(4) / reduce(2) conflict resolved as shift(4): precedence 1 == 1 associativity right at state(5) production(2) symbol(..)
|    |  id |  .. |  $  |  E' |  E  |
|  1 |  s3 |     |     |     |  s2 |
|  2 |     |  s4 | acc |     |     |
|  3 |     |  r3 |  r3 |     |     |
|  4 |  s3 |     |     |     |  s5 |
|  5 |     |  s4 |  r2 |     |     |
---------------------------------------------------------------------------
shift(4) / reduce(2) conflict resolved as an error: precedence 1 == 1 associativity nonassoc at state(5) production(2) symbol(==)
|    |  id |  == |  $  |  E' |  E  |
|  1 |  s3 |     |     |     |  s2 |
|  2 |     |  s4 | acc |     |     |
|  3 |     |  r3 |  r3 |     |     |
|  4 |  s3 |     |     |     |  s5 |
|  5 |     |     |  r2 |     |     |
---------------------------------------------------------------------------
reduce(2) / reduce(5) conflict resolved as reduce(2) at state(1) production(5) symbol($)
shift(4) / reduce(2) conflict resolved as shift(4) at state(1) production(2) symbol(id)
shift(4) / reduce(5) conflict resolved as shift(4) at state(1) production(5) symbol(id)
|    |  id |  $  |  S' |  S  |  K  |
|  1 |  s4 |  r2 |     |  s2 |  s3 |
|  2 |  s5 | acc |     |     |     |
|  3 |  r3 |  r3 |     |     |     |
|  4 |  r6 |  r6 |     |     |     |
|  5 |  r4 |  r4 |     |     |     |
---------------------------------------------------------------------------
|    |  id |  $  |  S' |  S  |
|  1 |  r2 |  r2 |     |  s2 |
|  2 |  s3 | acc |     |     |
|  3 |  r3 |  r3 |     |     |
]])
