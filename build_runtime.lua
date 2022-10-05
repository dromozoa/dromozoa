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

local quote_lua = require "dromozoa.quote_lua"

local source_filename, result_filename = ...

local handle = assert(io.open(source_filename))
local buffer = handle:read "*a"
handle:close()

local buffer = (buffer .. "$")
  :gsub("%-%-%[(%=*)%[.-%]%1%]", "")
  :gsub("%-%-[^\n]*", "")
  :gsub("[ \t]+\n", "\n")
  :gsub("\n\n+", "\n")
  :gsub("^\n+", "")
  :gsub("(.-)%$([%w_]*)", function (a, b)
    if b == "" then
      return quote_lua(a) .. ";\n"
    else
      return quote_lua(a) .. ";\n" .. "context[" .. quote_lua(b) .. "];\n"
    end
  end)

local out = assert(io.open(result_filename, "w"))
out:write("return function (context) return {\n", buffer, "} end\n")
out:close()
