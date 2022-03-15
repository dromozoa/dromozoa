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
local pattern = require "dromozoa.regexp.pattern"
local union = require "dromozoa.regexp.union"
local write_graphviz = require "dromozoa.regexp.write_graphviz"

local P = pattern.pattern
local S = pattern.set
local R = pattern.range

local debug = tonumber(os.getenv "DROMOZOA_TEST_DEBUG")
debug = debug and debug ~= 0

local definitions = {
  block_comment = guard {
    P(1);
  };

  main = union {
    (P"[" / "append_byte(0x5D)")
      * (P"=" / "append_byte(fc)")^0
      * (P"[" / "append_byte(0x5D)")
    % "fcall(block_comment)";
  };
}

for name, dfa in pairs(definitions) do
  local out = assert(io.open(("test-%s-dfa.dot"):format(name), "w"))
  write_graphviz(out, dfa)
  out:close()
end

local data = generate(definitions)
compile(io.stdout, data)


-- print(dumper.encode(data, { pretty = true, stable = true }))

