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

local list = require "dromozoa.list"
local pattern = require "dromozoa.regexp.pattern"
local machine = require "dromozoa.regexp.machine"
local compile = require "dromozoa.regexp.compile"

local _ = pattern
local any = pattern.any
local union = machine.union
local guard = machine.guard
local lexer = machine.lexer

local tokens = list()

local code = compile {
  escape_digit = union {
    (_["09"]/"ra=ra*10+fc-0x30"){0,2} % "fret()";
  };

  lexer(tokens, {
    -- comment
    _"--[[" + (any(){0} - (any(){0} + "]]" + any(){0})) + "]]";

    integer = _["09"]{1};
    string
      = _"\""
        +_{ _"\\"
            +_{ _["09"]/"ra=fc-0x30 fcall(escape_digit) append(fb,ra)"
            ; _"n"/"append(fb,'\\n')"
            }
        ; -_{"\\\""}/"append(fb,fc)"
        }{0}
      + "\"";
  });
}

local out = assert(io.open("test-lexer.lua", "w"))
out:write(code)
out:close()
