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

local array = require "dromozoa.array"
local pattern = require "dromozoa.regexp.pattern"
local machine = require "dromozoa.regexp.machine"
local compile = require "dromozoa.regexp.compile"

local _ = pattern
local union = machine.union
local guard = machine.guard
local lexer = machine.lexer

local tokens = array()

local code = compile {
  [[local ra]];

  digit = union {
    _["09"]/[[ra=ra*10+fc-0x30]]*-2 %[[fret()]];
  };

  comment = guard([[fret()]], {
    _"\n"/[[ln=ln+1 lp=fp]] + _"\r"/[[lp=fp]]*"?";
    _"\r"/[[ln=ln+1 lp=fp]] + _"\n"/[[lp=fp]]*"?";
    _"]" + _"="*"*" + ("]" + -_{"]\n\r"}*"*")*"?";
    -- _"]" + _"="*"*" + _"]"*"?";
    -- (_"]" + _"="*"*" + "]")*"?" + -_{"]\n\r"}*"+";
    -_{"]\n\r"}*"+";
  });

  lexer(tokens, {
    _{
      _{" \t\f\v"};
      _"\n"/[[ln=ln+1 lp=fp]] + _"\r"/[[lp=fp]]*"?";
      _"\r"/[[ln=ln+1 lp=fp]] + _"\n"/[[lp=fp]]*"?";
    }*"+";

    _"--" + _"["/[[append(fg,0x5D)]] + _"="/[[append(fg,fc)]]*"*" + _"["/[[append(fg,0x5D) fcall($comment)]];
    _"--" + -_{"\n\r"}*"*";

    string = (
      _[["]]/[[clear(fb)]] + _{
        _[[\]] + _["09"]/[[ra=fc-${<0>} fcall($digit) append(fb,ra)]];
        _[[\]] + _[[\]]/[[append(fb,${<\>})]];
        _[[\]] + _[["]]/[[append(fb,${<">})]];
        -_{[["\]]}/[[append(fb,fc)]]
      }*"*" + _[["]]
    ) %[[push(fb)]];

    _"*";
    _"+";
    integer = _["09"]*"+";
  });
}

local filename = "test-gen2.lua"
local out = assert(io.open(filename, "w"))
out:write(code)
out:close()

local buffer = array()

local execute = assert(assert(loadfile(filename))())
execute([[
--[=[
123] ]==]
]=]
-- test
+ 456
  * "foo\35abc" 111
]], filename, function (token)
  if token ~= nil then
    if token.symbol then
      buffer:append(table.concat({ "push", token.symbol, token.i, token.j, token.line, token.column, ("%q"):format(token.value) }, "\t") .. "\n")
    else
      buffer:append(table.concat({ "skip", "", token.i, token.j, token.line, token.column, ("%q"):format(token.source) }, "\t") .. "\n")
    end
  else
    buffer:append "push\t$\n"
  end
end)

-- print(buffer:concat())
assert(buffer:concat() == [[
skip		1	19	1	1	"--[=[\
123] ]==]\
]=]"
skip		20	20	3	4	"\
"
skip		21	27	4	1	"-- test"
skip		28	28	4	8	"\
"
push	3	29	29	5	1	"+"
skip		30	30	5	2	" "
push	4	31	33	5	3	"456"
skip		34	36	5	6	"\
  "
push	2	37	37	6	3	"*"
skip		38	38	6	4	" "
push	1	39	49	6	5	"foo#abc"
skip		50	50	6	16	" "
push	4	51	53	6	17	"111"
skip		54	54	6	20	"\
"
push	$
]])
