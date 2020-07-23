-- Copyright (C) 2020 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local node = require "dromozoa.lexer.node"
local dumper = require "dromozoa.commons.dumper"

local p = node.range "ac"
local p = node.set "ac"
-- local p = node.pattern "a" + "b" + "c"
local p = node.pattern "a" * "b" * "c"
-- local p = node.pattern "ab" * "c"
-- local p = node.pattern "abc"
-- local p = node.pattern "ab" + "c"
-- local p = node.pattern "ab" + 1
-- local p = - (- node.set "abc" + "c")
-- local p = (node.set "abc" ^ "?") ^ "+"
-- local p = node.pattern "abc"
-- local p = node.pattern "a" * "b" * "c"
-- local p = node.pattern "a"^-3
-- local p = node.pattern "a"^3
-- local p = node.pattern(3)
-- local p = node.pattern "a"^{3}
-- local p = node.pattern "a"^{0,4}
-- local p = node.set "abc" - node.set "b"
-- local p = node.set "abc" * node.set "def"
-- local p = node.set "abc\\]" * node.range " z"
-- local p = node.pattern(1)
-- local p = -node.set "a"
-- local p = node.pattern "abc" + (node.pattern "def" + node.pattern "ghi" ^ "+") ^ "?"
-- local p = node.pattern "abc" ^{3}
-- local p = node.pattern "あいうえお"
-- local p = (node.pattern "abc" ^"*")^"?"
-- local p = (node.pattern "abc" + "def") * "ghi"
-- local p = (node.pattern "a" ^"?") ^"*"

print(p:unparse())

local P = node.pattern
local R = node.range
local S = node.set

assert((P"abc"^"*"):unparse() == "(abc)*")
assert((S"abc"^"*"):unparse() == "[a-c]*")

-- local p1 = node.literal "foo"

local p = P"abc"^"?"
print(p:unparse())
print(dumper.encode(p:to_nfa()))
