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

local verbose = os.getenv "VERBOSE" == "1"

local array = require "dromozoa.array"
local pattern = require "dromozoa.regexp.pattern"
local machine = require "dromozoa.regexp.machine"
local compile = require "dromozoa.regexp.compile"

local _ = pattern
local union = machine.union
local guard = machine.guard
local lexer = machine.lexer

local token_names = array()
local code = compile {
  character_class = lexer(token_names, {
    ["]"] = _"]" %[[push() fret()]];
    escaped = _"\\" + _;
    -- char = _(_);
    char = -_{"\\]"};
  });

  lexer(token_names, {
    _{
      _{" \t\f\v"};
      _"\n"/[[ln=ln+1 lp=fp]] + _"\r"/[[lp=fp]]*"?";
      _"\r"/[[ln=ln+1 lp=fp]] + _"\n"/[[lp=fp]]*"?";
    }*"+";

    ["["] = _"[" %[[push() fcall($character_class)]];
    escaped = _"\\" + _;
    char = -_{"\\["};
  });
}

local buffer = array()
for _, name in token_names:ipairs() do
  buffer:append(name, "\n")
end

local filename = "test-gen3.lua"
local out = assert(io.open(filename, "w"))
out:write(code)
out:close()

local execute = assert(assert(loadfile(filename))())
execute([[
a ] b \1 c \[ d \] e
f [ g \1 h [ i \[ j \] k ] l
]], filename, function (token)
  if token ~= nil and token.symbol ~= nil then
    buffer:append("push ", token.symbol, " ", token_names:get(token.symbol), " ${<", token.value, ">}\n")
  end
end)

if verbose then
  print(buffer:concat())
end
assert(buffer:concat() == [[
]
escaped
char
[
push 3 char ${<a>}
push 3 char ${<]>}
push 3 char ${<b>}
push 2 escaped ${<\1>}
push 3 char ${<c>}
push 2 escaped ${<\[>}
push 3 char ${<d>}
push 2 escaped ${<\]>}
push 3 char ${<e>}
push 3 char ${<f>}
push 4 [ ${<[>}
push 3 char ${< >}
push 3 char ${<g>}
push 3 char ${< >}
push 2 escaped ${<\1>}
push 3 char ${< >}
push 3 char ${<h>}
push 3 char ${< >}
push 3 char ${<[>}
push 3 char ${< >}
push 3 char ${<i>}
push 3 char ${< >}
push 2 escaped ${<\[>}
push 3 char ${< >}
push 3 char ${<j>}
push 3 char ${< >}
push 2 escaped ${<\]>}
push 3 char ${< >}
push 3 char ${<k>}
push 3 char ${< >}
push 1 ] ${<]>}
push 3 char ${<l>}
]])
