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
local union = machine.union
local guard = machine.guard
local lexer = machine.lexer

local tokens = list()


--[=[

  -_{"\n"}
  ~_{"\n"}

  -_["09"]
  ~_["09"]


  _["09"]

  _"abc" "!"


  ..     (left)
  + -
  * / %
  # -    (unary)
  ^      (left)
  index call

  正規表現の優先順位
  |
  concat
  qunatifier *, +, ?, ...

  -- foo(bar+)baz
  _("foo", _"bar"{1}, "baz")


 _{ _{" \t\f\v"}
  ; _"\n"^[[ln=ln+1 lp=fp]] + _"\r"^[[lp=fp]]*{0,1}
  ; _"\r"^[[ln=ln+1 lp=fp]] + _"\n"^[[lp=fp]]*{0,1}
  }

    -_"\n\r"/[[lp=fp]]*{0,1}

--]=]

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

    _"--" + _"["/[[append(fg,0x5D)]] + _"="/[[append(fg,fc)]]*"*" + _"["/[[append(fg,0x5D) fcall(comment)]];
    _"--" + -_{"\n\r"}*"*";

    string = (
      _[["]]/[[clear(fb)]] + _{
        _[[\]] + _["09"]/[[ra=fc-0x30 fcall(digit) append(fb,ra)]];
        _[[\]] + _[[\]]/[[append(fb,0x5C)]];
        _[[\]] + _[["]]/[[append(fb,0x22)]];
        -_{[["\]]}/[[append(fb,fc)]]
      }*"*" + _[["]]
    ) %[[push(fb)]];

    _"*";
    _"+";
    integer = _["09"]*"+";
  });
}

local filename = "test-lexer.lua"
local out = assert(io.open(filename, "w"))
out:write(code)
out:close()

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
      print("push", token.symbol, token.i, token.j, token.line, token.column, ("%q"):format(token.value))
    else
      print("skip", "", token.i, token.j, token.line, token.column, ("%q"):format(token.source))
    end
  else
    print("push", "$")
  end
end)

