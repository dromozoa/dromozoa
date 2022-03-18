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
local loop = require "dromozoa.regexp.loop"
local pattern = require "dromozoa.regexp.pattern"

local P = pattern.pattern
local S = pattern.set
local R = pattern.range

local debug = tonumber(os.getenv "DROMOZOA_TEST_DEBUG")
debug = debug and debug ~= 0

local buffer = {}

function push(key)
  if debug then
    print(key)
  end
  buffer[#buffer + 1] = key
end

local out = assert(io.open("test.lua", "w"))
compile(out, generate {
  main = loop {
    (-S"\r\n")^0 % [[push "*"]];
    ((P"\r" / [[push "CR"]]) * (P"\n" / [[push "CRLF"]])^-1)^1 % [[push "x"]];
    ((P"\n" / [[push "LF"]]) * (P"\r" / [[push "LFCR"]])^-1)^1 % [[push "y"]];
  };
})
out:close()

local regexp = assert(loadfile "test.lua")()
regexp "a\rb\r\nc\nd\n\re\r\rf\n\ng"
assert(table.concat(buffer, ",") == "*,CR,x,*,CR,CRLF,x,*,LF,y,*,LF,LFCR,y,*,CR,CR,x,*,LF,LF,y,*")
