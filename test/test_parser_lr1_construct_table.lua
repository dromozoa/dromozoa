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

local append = require "dromozoa.append"
local grammar = require "dromozoa.parser.grammar"
local lalr = require "dromozoa.parser.lalr"

local _ = grammar.body
local expect = grammar.expect
local left = grammar.left
local right = grammar.right
local nonassoc = grammar.nonassoc

local G = {
  -- P.269 Example 4.60
  grammar({ "c", "d" }, {
    S = _"C" "C";
    C = _"c" "C"
      + _"d";
  });

  -- P.281 Figure 4.49
  grammar({ "id", "+", "*", "(", ")" }, {
    left "+";
    left "*";
    E = _"E" "+" "E"
      + _"E" "*" "E"
      + _"(" "E" ")"
      + _"id";
  });

  -- P.282 Figure 4.51
  grammar({ "i", "e", "a" }, {
    expect(1);
    S = _"i" "S" "e" "S"
      + _"i" "S"
      + _"a";
  });

  -- 右結合のテスト
  grammar({ "id", ".." }, {
    right "..";
    E = _"E" ".." "E"
      + _"id";
  });

  -- 無結合のテスト
  grammar({ "id", "==" }, {
    nonassoc "==";
    E = _"E" "==" "E"
      + _"id";
  });

  -- reduce/reduce衝突のテスト
  -- https://www.gnu.org/software/bison/manual/html_node/Reduce_002fReduce.html
  grammar({ "id" }, {
    expect(3); -- おおすぎるexpectのテスト
    S = _()
      + _"K"
      + _"S" "id";
    K = _()
      + _"id";
  });

  -- reduce/reduceが起こらない文法
  grammar({ "id" }, {
    S = _()
      + _"S" "id"
  });

  -- 生成規則の優先順位のテスト
  grammar({ "id", "-" } , {
    left "-";
    right "UNM";

    E = _"E" "-" "E"
      + _"-" "E" :prec "UNM"
      + _"id";
  });

  -- 二項演算子の優先順位を定義しわすれた場合のテスト
  grammar({ "id", "-" } , {
    right "UNM";

    E = _"E" "-" "E"
      + _"-" "E" :prec "UNM"
      + _"id";
  });
}

local buffer = {}

for _, g in ipairs(G) do
  append(buffer, ("-"):rep(75), "\n")
  local g, actions, conflictions = lalr(g)
  for _, message in ipairs(conflictions) do
    append(buffer, message, "\n")
  end

  append(buffer, "|    |")
  for i, name in ipairs(g.symbol_names) do
    append(buffer, ("  %-2s |"):format(name))
  end
  append(buffer, "\n")

  for i, data in ipairs(actions) do
    append(buffer, ("| %2d |"):format(i))
    for j in ipairs(g.symbol_names) do
      local v = data[j]
      if v == 0 then
        append(buffer, "     |")
      else
        append(buffer, " ")
        if v <= #actions then
          append(buffer, ("%3s"):format("s" .. v))
        else
          local v = v - #actions
          if v == 1 then
            append(buffer, "acc")
          else
            append(buffer, ("%3s"):format("r" .. v))
          end
        end
        append(buffer, " |")
      end
    end
    append(buffer, "\n")
  end

end

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
[info] conflict between production 2 and symbol + resolved as reduce (left +)
[info] conflict between production 2 and symbol * resolved as shift (+ < *)
[info] conflict between production 3 and symbol + resolved as reduce (+ < *)
[info] conflict between production 3 and symbol * resolved as reduce (left *)
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
[info] state 5 conflicts: 1 shift/reduce
[info] shift/reduce conflicts: 1 found, 1 expected
|    |  i  |  e  |  a  |  $  |  S' |  S  |
|  1 |  s3 |     |  s4 |     |     |  s2 |
|  2 |     |     |     | acc |     |     |
|  3 |  s3 |     |  s4 |     |     |  s5 |
|  4 |     |  r4 |     |  r4 |     |     |
|  5 |     |  s6 |     |  r3 |     |     |
|  6 |  s3 |     |  s4 |     |     |  s7 |
|  7 |     |  r2 |     |  r2 |     |     |
---------------------------------------------------------------------------
[info] conflict between production 2 and symbol .. resolved as shift (right ..)
|    |  id |  .. |  $  |  E' |  E  |
|  1 |  s3 |     |     |     |  s2 |
|  2 |     |  s4 | acc |     |     |
|  3 |     |  r3 |  r3 |     |     |
|  4 |  s3 |     |     |     |  s5 |
|  5 |     |  s4 |  r2 |     |     |
---------------------------------------------------------------------------
[info] conflict between production 2 and symbol == resolved as an error (nonassoc ==)
|    |  id |  == |  $  |  E' |  E  |
|  1 |  s3 |     |     |     |  s2 |
|  2 |     |  s4 | acc |     |     |
|  3 |     |  r3 |  r3 |     |     |
|  4 |  s3 |     |     |     |  s5 |
|  5 |     |     |  r2 |     |     |
---------------------------------------------------------------------------
[warn] state 1 conflicts: 2 shift/reduce, 1 reduce/reduce
[warn] shift/reduce conflicts: 2 found, 3 expected
[warn] reduce/reduce conflicts: 1 found
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
---------------------------------------------------------------------------
[info] conflict between production 3 and symbol - resolved as reduce (- < UNM)
[info] conflict between production 2 and symbol - resolved as reduce (left -)
|    |  id |  -  |  $  |  E' |  E  |
|  1 |  s4 |  s3 |     |     |  s2 |
|  2 |     |  s5 | acc |     |     |
|  3 |  s4 |  s3 |     |     |  s6 |
|  4 |     |  r4 |  r4 |     |     |
|  5 |  s4 |  s3 |     |     |  s7 |
|  6 |     |  r3 |  r3 |     |     |
|  7 |     |  r2 |  r2 |     |     |
---------------------------------------------------------------------------
[info] conflict between production 3 and symbol - resolved as reduce (- < UNM)
[warn] state 7 conflicts: 1 shift/reduce
[warn] shift/reduce conflicts: 1 found
|    |  id |  -  |  $  |  E' |  E  |
|  1 |  s4 |  s3 |     |     |  s2 |
|  2 |     |  s5 | acc |     |     |
|  3 |  s4 |  s3 |     |     |  s6 |
|  4 |     |  r4 |  r4 |     |     |
|  5 |  s4 |  s3 |     |     |  s7 |
|  6 |     |  r3 |  r3 |     |     |
|  7 |     |  s5 |  r2 |     |     |
]])
