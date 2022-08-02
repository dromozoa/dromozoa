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

local compile = require "dromozoa.parser.compile"
local grammar = require "dromozoa.parser.grammar"
local parser = require "dromozoa.parser.parser"

local _ = grammar.body
local left = grammar.left

local g = grammar({ "id", "+", "*", "(", ")" }, {
  left "+";
  left "*";

  E = _"E" "+" "E"
    + _"E" "*" "E"
    + _"(" "E" ")"
    + _"id";
})

local p = parser(g, function (message)
  print(message)
end)

local code = compile(p)

local filename = "test-gen-parser.lua"
local out = assert(io.open(filename, "w"))
out:write(code)
out:close()
