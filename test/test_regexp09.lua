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
local union = require "dromozoa.regexp.union"

local P = pattern.pattern
local S = pattern.set
local R = pattern.range

local out = assert(io.open("test-gen.lua", "w"))
local token_names = {}
local data = generate {
  main = lexer(token_names, {
    P"and";
    P"or";

    S" \t\r\n"^1 % "skip_token()";

    IntegerConstant = (
        R"09"/[[ra=fc-0x30]] * (R"09"/[[ra=ra*10+fc-0x30]])^0
      + P"0" * (P"x"/[[ra=0]]) * (R"09"/[[ra=ra*16+fc-0x30]] + R"AF"/[[ra=ra*16+fc-0x41+10]] + R"af"/[[ra=ra*16+fc-0x61+10]])^1
    ) % "push_token(ra)";
  });
}
compile(out, data)
out:close()

for i = 1, #token_names do
  print(i, token_names[i])
end

local regexp = assert(loadfile "test-gen.lua")()
regexp [[
123 and 456 or 0xAf and
]]
-- assert(table.concat(buffer, ",") == "*,CR,x,*,CR,CRLF,x,*,LF,y,*,LF,LFCR,y,*,CR,CR,x,*,LF,LF,y,*")
