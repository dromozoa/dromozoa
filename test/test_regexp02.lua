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

local fsm = require "dromozoa.regexp.fsm"
local pattern = require "dromozoa.regexp.pattern"
local tree_to_nfa = require "dromozoa.regexp.tree_to_nfa"
local nfa_to_dfa = require "dromozoa.regexp.nfa_to_dfa"
local minimize = require "dromozoa.regexp.minimize"
local write_graphviz = require "dromozoa.regexp.write_graphviz"

local P = pattern.pattern
local S = pattern.set
local R = pattern.range

--[=[
  1. Ragelの:=に相当することがしたい
  2. 良い感じにaccept番号を割り当てたい
  3. DFAになってからのunionは欲しい


  1について、Ragelの場合、
    machine definition    <name> = <expression>
    machine instantiation <name> := <expression>
  と定義されている。

  machine definitionはLuaの変数代入で十分（名前の参照解決が必要ないので）。

  machine instantiationは、名前をどこかに記録したうえで、DFAに変換する処理とす
  るか？

  単純に考えると、名前は入口を指示し、accept番号はマシンの出口を指示する。
  さらに、マシンの出口にはなんらかのアクションが指定される。

  名前は順序づけられる必要がなく、accept番号は順序づけが欲しい

  呼び出されないのであれば、名前をつける必要はない
  例: unionしちゃうとか

  instantiate(pat, "name", 42) => fsm

  union { fsm, fsm, fsm }

  _:guard "string_literal" {
    _:action { "fret;" },
    P(1) / _:action { "append_buf(c)" },
  }

  _:union "main" {
    P"[" / _:action { "clear_gbuf"; "append_gbuf(']')" }
      * (P"=" / _:action { "append_gbuf(c)" })^0
      * P"[" / _:action { "fcall(string_literal)" };

    pat42 % "fcall(test)";
    pat69 % 69;
  }

local literal = P[["]] * (-S[["\]])^0 * P[["]]
local comment = P"/*" * (P(1)^0 - P(1)^0 * P"*/" * P(1)^0) * P"*/"
local literal_or_comment = literal + comment

]=]

local function machine(root, accept)
  local nfa = tree_to_nfa(root, accept)
  local dfa1 = nfa_to_dfa(nfa)
  local dfa2 = minimize(dfa1)
  return dfa2
end

local s1 = machine(P"if", "if")
local s2 = machine(P"else", "else")
local s3 = machine(P"elif", "elseif")
local s4 = machine(P"elsif", "elseif")
local s5 = machine(P"elseif", "elseif")
local s6 = machine(P"end", "end")
local s7 = machine(R"AZaz" * R"09AZaz"^0, "ID")

local s = fsm.new_state()
fsm.new_transition(s, s1)
fsm.new_transition(s, s2)
fsm.new_transition(s, s3)
fsm.new_transition(s, s4)
fsm.new_transition(s, s5)
fsm.new_transition(s, s6)
fsm.new_transition(s, s7)

local dfa1 = nfa_to_dfa(s)
local dfa2 = minimize(dfa1)

local out = assert(io.open("test-nfa.dot", "w"))
write_graphviz(out, s)
out:close()

local out = assert(io.open("test-dfa1.dot", "w"))
write_graphviz(out, dfa1)
out:close()

local out = assert(io.open("test-dfa2.dot", "w"))
write_graphviz(out, dfa2)
out:close()
