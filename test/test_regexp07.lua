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
local loop = require "dromozoa.regexp.loop"
local pattern = require "dromozoa.regexp.pattern"
local union = require "dromozoa.regexp.union"
local write_graphviz = require "dromozoa.regexp.write_graphviz"

local P = pattern.pattern
local S = pattern.set
local R = pattern.range

local debug = tonumber(os.getenv "DROMOZOA_TEST_DEBUG")
debug = debug and debug ~= 0

local definitions = {
  string_literal = union {
    P[["]] / [[p "\""]] % [[fret()]];
    (R"az" / [[p "char"]])^1 % [[fgoto(string_literal)]];
  };

  block_comment = guard("fret()", {
    (-S"]") % [[p "comment char"]];
  });

  main = loop {
    P"if"     % [[p "if"]];
    P"else"   % [[p "else"]];
    P"elseif" % [[p "elseif"]];
    P"end"    % [[p "end"]];
    P"then"   % [[p "then"]];
    P"local"  % [[p "local"]];
    P"--"
      * (P"[" / [[guard_assign "]"]])
      * (P"=" / [[guard_append()]])^0
      * (P"[" / [[guard_append "]" fcall(block_comment)]])
      % [[p "block comment"]];
    ((P"--" * (-S"\r\n")^0 * (P"\r\n" + P"\r" + P"\n")) - (P"--[" * P"="^0 * P"[" * P(1)^0))
              % [[p "line comment"]];
    P[["]] / [[fcall(string_literal)]] % [[p "string_literal"]];
    R"AZaz__" * R"09AZaz__"^0 % [[p "id"]];
    P"=" % [[p "="]];
    R"09"^1 % [[p "int"]];
    S" \t\r\n"^1;
  };
}

for name, dfa in pairs(definitions) do
  local out = assert(io.open(("test-%s-dfa.dot"):format(name), "w"))
  write_graphviz(out, dfa)
  out:close()
end

local out = assert(io.open("test.lua", "w"))
compile(out, generate(definitions))
out:close()

if debug then
  p = print
else
  function p() end
end

local regexp = assert(loadfile "test.lua")()
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
