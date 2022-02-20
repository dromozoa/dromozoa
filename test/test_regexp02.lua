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

--[[
  正規表現 (DFA) レキサ

  正規表現でないレキサは後で実装する
  レキサの生成自体はFull Luaで実装してよい
  生成されたコードはTiny Luaで実装する
]]

-- set "abc" -- [abc]
-- range "az" -- [a-z]

local regexp = require "dromozoa.regexp"
local tree_to_nfa = require "dromozoa.regexp.tree_to_nfa"
local nfa_to_dfa = require "dromozoa.regexp.nfa_to_dfa"
local minimize = require "dromozoa.regexp.minimize"
local write_graphviz_tree = require "dromozoa.regexp.write_graphviz_tree"
local write_graphviz = require "dromozoa.regexp.write_graphviz"

local P = regexp.pattern.pattern
local R = regexp.pattern.range
local S = regexp.pattern.set

local p = P(1) * P"abc" * (R"09" + S"abc")
local p = P(2) * P"abc"^0
local p = P"abc"^0
local p = P"abc"^1
local p = (R"ac" * P"abc" + (P"d" / 1 + R"df" / 2) * P"def")^0
local p = R"07" * R"07"^-2 * P(1)

-- local p = "<" * ("\\" * R"09" * R"09"^-2 * "X" + R"az" * "Y")^0 * ">"
local p = P"\"" * (P"\\" * R"09" * R"09"^-2 % 1 + (R"az" / 2)^1)^0 * P"\""

-- write_graphviz_tree(io.stdout, p)
-- write_graphviz(io.stdout, tree_to_nfa(p, 42))
local dfa = nfa_to_dfa(tree_to_nfa(p, 1))
-- local dfa = minimize(dfa)
write_graphviz(io.stdout, dfa)

