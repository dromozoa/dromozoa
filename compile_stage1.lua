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
local stage1 = require "dromozoa.compiler.stage1"
local source_map = require "dromozoa.compiler.source_map"

local result_filename, source_map_filename, runtime_filename, main_filename = ...

local function parse(filename)
  local handle = assert(io.open(filename))
  local source = handle:read "*a"
  handle:close()
  return generate(lua54_regexp(source, filename, lua54_parser.max_terminal_symbol, lua54_parser()))
end

local chunks = { parse(runtime_filename) }
local chunk_filenames = { main_filename }

for i, filename in ipairs(chunk_filenames) do
  local handle = assert(io.open(filename))
  local source = handle:read "*a"
  handle:close()

  local root = lua54_regexp(source, filename, lua54_parser.max_terminal_symbol, lua54_parser())
  local chunk = generate(root)
  append(chunks, chunk)

  for _, module_name in ipairs(chunk.static_require) do
    local chunk_filename = module_name:gsub("%.", "/") .. ".lua"
    if not chunk_filenames[chunk_filename] then
      chunk_filenames[chunk_filename] = append(chunk_filenames, chunk_filename)
    end
  end
end

local result = {}
local source_map = source_map(result_filename)
stage1.generate_prologue(result, source_map)
for _, chunk in ipairs(chunks) do
  stage1.generate_chunk(result, source_map, chunk)
end
stage1.generate_epilogue(result, source_map, source_map_filename)

local out = assert(io.open(result_filename, "w"))
out:write(table.concat(result))
out:close()

local out = assert(io.open(source_map_filename, "w"))
out:write(table.concat(source_map:generate()))
out:close()
