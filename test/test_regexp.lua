-- Copyright (C) 2020-2022 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local verbose = os.getenv "VERBOSE" == "1"

local append = require "dromozoa.append"
local pattern = require "dromozoa.regexp.pattern"
local machine = require "dromozoa.regexp.machine"
local compile = require "dromozoa.regexp.compile"

local _ = pattern
local union = machine.union
local guard = machine.guard
local lexer = machine.lexer

local token_names = {}

local code = compile {
  B = union {
    _"B"/"append(fc)"*{0,2} %"freturn()";
  };

  A = union {
    _"A"/"append(fc)"*{0,2} %"freturn()";
    _"B"%"append(fc) fcall($B) freturn()";
  };

  lexer(token_names, {
    _" "*"+";

    A = _"A"%"clear() append(fc) fcall($A) push(true)";
  });
}

local filename = "out/test_regexp.lua"
local out = assert(io.open(filename, "w"))
out:write(code)
out:close()

local buffer = {}

local execute = assert(assert(loadfile(filename))())
execute("A AA AAA AAAA AB ABB ABBB ABBBAAAAB ", "@test", 0, function (token)
  local symbol = token[0]
  if symbol then
    if symbol == 0 then
      append(buffer, "push\t$\n")
    else
      append(buffer, "push\t", symbol, "\t", ("%q"):format(token.v), "\n")
    end
  else
    append(buffer, "skip\t", ("%q"):format(token.s), "\n")
  end
end)

print(table.concat(buffer))
