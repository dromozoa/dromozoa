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

local function parse(filename)
  local handle = assert(io.open(filename))
  local source = handle:read "*a"
  handle:close()
  return generate(lua54_regexp(source, filename, lua54_parser.max_terminal_symbol, lua54_parser()))
end

local function preload(modules, chunk)
  for _, name in ipairs(chunk.static_require) do
    if not modules[name] then
      local chunk = parse(name:gsub("%.", "/") .. ".lua")
      modules[name] = append(modules, { name = name, chunk = chunk })
      preload(modules, chunk)
    end
  end
end

local result_filename, source_root, runtime_filename, main_filename = ...
local source_map_filename = result_filename .. ".map"
local source_map_basename = source_map_filename:gsub(".*%/", "")

local runtime_chunk = parse(runtime_filename)
local main_chunk = parse(main_filename)
local modules = {}
preload(modules, main_chunk)

local result = {}
local source_map = source_map(source_root)
stage1.generate_prologue(result, source_map)
stage1.generate_chunk(result, source_map, runtime_chunk)
for _, module in ipairs(modules) do
  stage1.generate_module(result, source_map, module.name, module.chunk)
end
stage1.generate_chunk(result, source_map, main_chunk)
stage1.generate_epilogue(result, source_map, source_map_basename)

local out = assert(io.open(result_filename, "w"))
out:write(table.concat(result))
out:close()

local out = assert(io.open(source_map_filename, "w"))
out:write(table.concat(source_map:generate()))
out:close()
