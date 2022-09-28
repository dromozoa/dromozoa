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
local compiler_error = require "dromozoa.compiler.compiler_error"
local lua54_regexp = require "dromozoa.compiler.lua54_regexp"
local lua54_parser = require "dromozoa.compiler.lua54_parser"
local generate = require "dromozoa.compiler.generate"
local generate_stage1 = require "dromozoa.compiler.generate_stage1"
local quote = require "dromozoa.compiler.quote"
local source_map = require "dromozoa.compiler.source_map"

---------------------------------------------------------------------------

local source_filename, result_filename, source_map_filename = ...

local handle = assert(io.open(source_filename))
local source = handle:read "*a"
handle:close()

local parse = lua54_parser()
local root = lua54_regexp(source, source_filename, lua54_parser.max_terminal_symbol, parse)
local protos = generate(root)
local result = {}
local source_map = source_map(result_filename)
generate_stage1(protos, result, source_map)

local out = assert(io.open(result_filename, "w"))
out:write(table.concat(result), "//# sourceMappingURL=", source_map_filename, "\n")
out:close()

---------------------------------------------------------------------------

local out = assert(io.open(source_map_filename, "w"))
out:write(table.concat(source_map:generate()))
out:close()
