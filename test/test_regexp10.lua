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
local guard = require "dromozoa.regexp.guard"
local lexer = require "dromozoa.regexp.lexer"
local pattern = require "dromozoa.regexp.pattern"

local P = pattern.pattern
local S = pattern.set
local R = pattern.range

local out = assert(io.open("test-gen.lua", "w"))
local token_names = {}
compile(out, generate {

  block_comment = guard([[fret()]], {
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
    * (P"["/[[assign(fg,"]")]])
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
local tokens = regexp([[
if x then
  local i = { 42, -1, 0xAf, 0XaF }
  local f = {
    0.5, .5, 0.,
    0xAf.5, 0x.5, 0xAf.,
    0.5e2, 0.5e+2, 0.5e-2,
    0xAfp2, 0xAfp+2, 0xAfp-2,
  }
  --[=[ test
  ]=]
  return 3.14
end
]], "(string)")

for i = 1, #tokens do
  local tk = tokens[i]
  if tk.symbol then
    if debug then
      print(("%d %q %s"):format(tk.symbol, tk.source, tk.value))
    end
  end
end
