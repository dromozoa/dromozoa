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

local append = require "dromozoa.append"
local lua54_regexp = require "dromozoa.compiler.lua54_regexp"
local lua54_parser = require "dromozoa.compiler.lua54_parser"
local generate = require "dromozoa.compiler.generate"
local generate_stage1 = require "dromozoa.compiler.generate_stage1"
local source_map = require "dromozoa.compiler.source_map"
local table_unpack = table.unpack or unpack

local result_filename, source_map_filename = ...

local set_of_protos = {}

for i = 3, #arg do
  local filename = arg[i]
  local handle = assert(io.open(filename))
  local source = handle:read "*a"
  handle:close()

  local root = lua54_regexp(source, filename, lua54_parser.max_terminal_symbol, lua54_parser())
  local protos = generate(root)
  append(set_of_protos, protos)
end

local result = {}
local source_map = source_map(result_filename)
generate_stage1(result, source_map, table_unpack(set_of_protos))

local out = assert(io.open(result_filename, "w"))
out:write(table.concat(result), "//# sourceMappingURL=", source_map_filename, "\n")
out:close()

local out = assert(io.open(source_map_filename, "w"))
out:write(table.concat(source_map:generate()))
out:close()
