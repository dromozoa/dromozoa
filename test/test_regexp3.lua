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
  character_class = lexer(token_names, {
    ["]"] = _"]" %[[push() freturn()]];
    escaped = _"\\" + _();
    char = _();
  });

  lexer(token_names, {
    _{
      _{" \t\f\v"};
      _"\n"/[[ln=ln+1 lp=fp]] + _"\r"/[[lp=fp]]*"?";
      _"\r"/[[ln=ln+1 lp=fp]] + _"\n"/[[lp=fp]]*"?";
    }*"+";

    ["["] = _"[" %[[push() fcall($character_class)]];
    escaped = _"\\" + _();
    char = _();
  });
}

local buffer = {}
for _, name in ipairs(token_names) do
  append(buffer, name, "\n")
end

local filename = "test-gen3.lua"
local out = assert(io.open(filename, "w"))
out:write(code)
out:close()

local execute = assert(assert(loadfile(filename))())
execute([[
a ] b \1 c \[ d \] e
f [ g \1 h [ i \[ j \] k ] l
]], "@test", 0, function (token)
  if token[0] ~= nil then
    if token[0] ~= 0 then
      append(buffer, "push ", token[0], " ", token_names[token[0]], " ${<", token.v, ">}\n")
    else
      append(buffer, "push eof\n")
    end
  end
end)

assert(table.concat(buffer) == [[
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
push eof
]])
