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

local compile = require "dromozoa.regexp.compile"
local generate = require "dromozoa.regexp.generate"
local guard = require "dromozoa.regexp.guard"
local lexer = require "dromozoa.regexp.lexer"
local pattern = require "dromozoa.regexp.pattern"
local union = require "dromozoa.regexp.union"

local P = pattern.pattern
local S = pattern.set
local R = pattern.range

local out = assert(io.open("test-gen.lua", "w"))
local token_names = {}

local quoted_char =
( P[[\]] *
    ( P[[a]]/[[append(fb,"\a")]]
    + P[[b]]/[[append(fb,"\b")]]
    + P[[f]]/[[append(fb,"\f")]]
    + P[[n]]/[[append(fb,"\n")]]
    + P[[r]]/[[append(fb,"\r")]]
    + P[[t]]/[[append(fb,"\t")]]
    + P[[v]]/[[append(fb,"\v")]]
    + P[[\]]/[[append(fb,"\\")]]
    + P[["]]/[[append(fb,"\"")]]
    + P[[']]/[[append(fb,"\'")]]
    + P"\r"/[[ln=ln+1 lp=fp append(fb,"\n")]] * (P"\n"/[[lp=fp]])^-1
    + P"\n"/[[ln=ln+1 lp=fp append(fb,"\n")]] * (P"\r"/[[lp=fp]])^-1
    + P[[z]] *
      ( S" \t"
      + P"\r"/[[ln=ln+1 lp=fp]] * (P"\n"/[[lp=fp]])^-1
      + P"\n"/[[ln=ln+1 lp=fp]] * (P"\r"/[[lp=fp]])^-1
      )
    + R"09"/[[ra=fc-0x30 fcall(escaped_decimal)]]
    + P[[x]] *
      ( R"09"/[[ra=fc-0x30]]
      + R"AF"/[[ra=fc-0x41+10]]
      + R"af"/[[ra=fc-0x61+10]]
      ) *
      ( R"09"/[[append(fb,ra*16+fc-0x30)]]
      + R"AF"/[[append(fb,ra*16+fc-0x41+10)]]
      + R"af"/[[append(fb,ra*16+fc-0x61+10)]]
      )
    + P[[u]] * (P"{"/[[ra=0]]) *
      ( R"09"/[[ra=ra*16+fc-0x30]]
      + R"AF"/[[ra=ra*16+fc-0x41+10]]
      + R"af"/[[ra=ra*16+fc-0x61+10]]
      )^1 * (P"}"/[[append_utf8(fb,ra)]])
    )
+ (-S[[\"']])/[[append(fb)]]
)

compile(out, generate {
  escaped_decimal = union {
    (R"09"/[[ra=ra*10+fc-0x30]])^-2 %[[append(fb,ra) fret()]]
  };

  long_string = guard([[fret()]], {
    ( P"\r"/[[ln=ln+1 lp=fp]] * (P"\n"/[[lp=fp]])^-1
    + P"\n"/[[ln=ln+1 lp=fp]] * (P"\r"/[[lp=fp]])^-1
    ) %[[if ra==1 then append(fb,"\n") end ra=1]];

    (-S"\r\n]")^1 %[[append_range(fb) ra=1]];

    P(1) %[[append(fb) ra=1]];
  });

  block_comment = guard([[fret()]], {
    ( P"\r"/[[ln=ln+1 lp=fp]] * (P"\n"/[[lp=fp]])^-1
    + P"\n"/[[ln=ln+1 lp=fp]] * (P"\r"/[[lp=fp]])^-1
    );

    (-S"\r\n]")^1;

    P(1);
  });

  main = lexer(token_names, {
    P"and";
    P"break";
    P"do";
    P"else";
    P"elseif";
    P"end";
    P"false";
    P"for";
    P"function";
    P"goto";
    P"if";
    P"in";
    P"local";
    P"nil";
    P"not";
    P"or";
    P"repeat";
    P"return";
    P"then";
    P"true";
    P"until";
    P"while";

    LiteralString =
    ( P[["]]/[[clear(fb)]] * (quoted_char + P[[']]/[[append(fb)]])^0 * P[["]]
    + P[[']]/[[clear(fb)]] * (quoted_char + P[["]]/[[append(fb)]])^0 * P[[']]
    + (P"["/[[clear(fg) append(fg,"]")]])
    * (P"="/[[append(fg)]])^0
    * (P"["/[[append(fg,"]") clear(fb) ra=0 fcall(long_string)]])
    ) %[[push_token(fb)]];

    P"+";
    P"-";
    P"*";
    P"/";
    P"%";
    P"^";
    P"#";
    P"&";
    P"~";
    P"|";
    P"<<";
    P">>";
    P"//";
    P"==";
    P"~=";
    P"<=";
    P">=";
    P"<";
    P">";
    P"=";
    P"(";
    P")";
    P"{";
    P"}";
    P"[";
    P"]";
    P"::";
    P";";
    P":";
    P",";
    P".";
    P"..";
    P"...";

    ( S" \t"
    + P"\r"/[[ln=ln+1 lp=fp]] * (P"\n"/[[lp=fp]])^-1
    + P"\n"/[[ln=ln+1 lp=fp]] * (P"\r"/[[lp=fp]])^-1
    )^1 %[[skip_token()]];

    Name = R"AZaz__" * R"09AZaz__"^0;

    IntegerConstant =
    ( R"09"/[[ra=fc-0x30]] * (R"09"/[[ra=ra*10+fc-0x30]])^0
    + P"0" * (S"Xx"/[[ra=0]]) *
      ( R"09"/[[ra=ra*16+fc-0x30]]
      + R"AF"/[[ra=ra*16+fc-0x41+10]]
      + R"af"/[[ra=ra*16+fc-0x61+10]]
      )^1
    ) %[[push_token(ra)]];

    FloatConstant =
    ( (R"09"^1 * (P"." * R"09"^0)^-1 + P"." * R"09"^1) * (S"Ee" * S"+-"^-1 * R"09"^1)^-1
    + P"0" * S"Xx" * (R"09AFaf"^1 * (P"." * R"09AFaf"^0)^-1 + P"." * R"09AFaf"^1) * (S"Pp" * S"+-"^-1 * R"09"^1)^-1
    );

    P"--"
    * (P"["/[[clear(fg) append(fg,"]")]])
    * (P"="/[[append(fg)]])^0
    * (P"["/[[append(fg,"]") fcall(block_comment)]])
    %[[skip_token()]];

    P"--" * ((-S"\r\n")^0 - P"[" * P"="^0 * P"[" * P(1)^0) %[[skip_token()]];
  });
})
out:close()

if debug then
  for i = 1, #token_names do
    print(i, token_names[i])
  end
end

local regexp = assert(loadfile "test-gen.lua")()
local tokens = regexp([==[
if x then
  local i = { 42, -1, 0xAf, 0XaF }
  local f = {
    0.5, .5, 0.,
    0xAf.5, 0x.5, 0xAf.,
    0.5e2, 0.5e+2, 0.5e-2,
    0xAfp2, 0xAfp+2, 0xAfp-2,
  }
  --[=[ test [[test]]
  ]=]
  local s = [[
abc
def]]
  return "3\t14\u{1F914}\n"
end
]==], "(string)")

for i = 1, #tokens do
  local tk = tokens[i]
  if tk.symbol then
    if debug then
      print(("%d:%d: %d %q %s"):format(tk.line, tk.column, tk.symbol, tk.source, tk.value))
    end
  end
end
