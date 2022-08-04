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
local write_graphviz = require "dromozoa.regexp.write_graphviz"

local P = pattern.pattern
local S = pattern.set
local R = pattern.range

local token_names = {}
local data = {
  escaped_decimal = union {
    (R"09"/[[print "/ed"]])^-2 %[[print "%ed" fret()]];
  };

  main = lexer(token_names, {
    string = P[["]] *
      ( P[[\]] *
        ( P[[n]]/[[print "/n"]]
        + P[[r]]/[[print "/r"]]
        + P[[t]]/[[print "/t"]]
        + R"09"/[[print "/ed0" fcall(escaped_decimal)]]
        )
      + (-S[[\"]])
    )^0 * P[["]];
  });
}

local out = assert(io.open("test-dfa1.dot", "w"))
write_graphviz(out, data.escaped_decimal)
out:close()

local out = assert(io.open("test-dfa2.dot", "w"))
write_graphviz(out, data.main)
out:close()

local out = assert(io.open("test-gen.lua", "w"))
compile(out, generate(data))
out:close()

if debug then
  for i = 1, #token_names do
    print(i, token_names[i])
  end
end

local regexp = assert(loadfile "test-gen.lua")()
local tokens = regexp([["\nabc\12\tdef"]], "(string)")

for i = 1, #tokens do
  local tk = tokens[i]
  if tk.symbol then
    if debug then
      print(("%d:%d: %d %q %s"):format(tk.line, tk.column, tk.symbol, tk.source, tk.value))
    end
  end
end
