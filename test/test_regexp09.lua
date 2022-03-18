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

local compile = require "dromozoa.regexp.compile"
local generate = require "dromozoa.regexp.generate"
local lexer = require "dromozoa.regexp.lexer"
local pattern = require "dromozoa.regexp.pattern"

local P = pattern.pattern
local S = pattern.set
local R = pattern.range

local out = assert(io.open("test.lua", "w"))
compile(out, generate {
  main = lexer {
    P"and";
    P"or";
    fuck = P"fuck" % "push_token(42)";

    WhiteSpace = S" \t\r\n"^1 % "skip_token()";

    -- IntegerConstant = union {
    --   R"09" * R"09"^0 % "push_token(v)"
    --   P"0x" * (R"09" + R"AF" + R"af")^1 % "push_token(v)"
    -- }
  };
})
out:close()

-- local regexp = assert(loadfile "test.lua")()
-- regexp "a\rb\r\nc\nd\n\re\r\rf\n\ng"
-- assert(table.concat(buffer, ",") == "*,CR,x,*,CR,CRLF,x,*,LF,y,*,LF,LFCR,y,*,CR,CR,x,*,LF,LF,y,*")
