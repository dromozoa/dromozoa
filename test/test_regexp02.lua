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

local minimize = require "dromozoa.regexp.minimize"
local nfa_to_dfa = require "dromozoa.regexp.nfa_to_dfa"
local pattern = require "dromozoa.regexp.pattern"
local union = require "dromozoa.regexp.union"
local write_graphviz = require "dromozoa.regexp.write_graphviz"

local P = pattern.pattern
local S = pattern.set
local R = pattern.range

local nfa = union {
  P"if" % "if";
  P"else" % "else";
  P"elif" % "elseif";
  P"elsif" % "elseif";
  P"elseif" % "elseif";
  P"end" % "end";
  R"AZaz" * R"09AZaz"^0 % "ID";
}

local dfa1 = nfa_to_dfa(nfa)
local dfa2 = minimize(dfa1)

local out = assert(io.open("test-nfa.dot", "w"))
write_graphviz(out, nfa)
out:close()

local out = assert(io.open("test-dfa1.dot", "w"))
write_graphviz(out, dfa1)
out:close()

local out = assert(io.open("test-dfa2.dot", "w"))
write_graphviz(out, dfa2)
out:close()
