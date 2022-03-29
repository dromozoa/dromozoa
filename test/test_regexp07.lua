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
local union = require "dromozoa.regexp.union"
local write_graphviz = require "dromozoa.regexp.write_graphviz"

local P = pattern.pattern
local S = pattern.set
local R = pattern.range

local debug = tonumber(os.getenv "DROMOZOA_TEST_DEBUG")
debug = debug and debug ~= 0

local data = {
  string_literal = union {
    P[["]] / [[print "\""]] % [[fret()]];
    (R"az" / [[print "char"]])^1 % [[fgoto(string_literal)]];
  };

  block_comment = guard("fret()", {
    (-S"]") % [[print "comment char"]];
  });

  main = lexer({}, {
    P"if"     % [[print "if"]];
    P"else"   % [[print "else"]];
    P"elseif" % [[print "elseif"]];
    P"end"    % [[print "end"]];
    P"then"   % [[print "then"]];
    P"local"  % [[print "local"]];
    P"--"
      * (P"[" / [[clear(fg) append(fg,"]")]])
      * (P"=" / [[append(fg)]])^0
      * (P"[" / [[append(fg,"]") fcall(block_comment)]])
      % [[print "block comment"]];
    ((P"--" * (-S"\r\n")^0 * (P"\r\n" + P"\r" + P"\n")) - (P"--[" * P"="^0 * P"[" * P(1)^0))
              % [[print "line comment"]];
    P[["]] / [[fcall(string_literal)]] % [[print "string_literal"]];
    R"AZaz__" * R"09AZaz__"^0 % [[print "id"]];
    P"=" % [[print "="]];
    R"09"^1 % [[print "int"]];
    S" \t\r\n"^1;
  });
}

for name, dfa in pairs(data) do
  local out = assert(io.open(("test-%s-dfa.dot"):format(name), "w"))
  write_graphviz(out, dfa)
  out:close()
end

local out = assert(io.open("test-gen.lua", "w"))
compile(out, generate(data))
out:close()

local save = print
if not debug then
  print = function () end
end

local regexp = assert(loadfile "test-gen.lua")()
regexp [[
-- test
if then elseif else
"aaa" end
local xyz = 123
--[=[
  abc
]=]
local abc
]]
