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

local list = require "dromozoa.list"

local function build(source, result)
  local handle = assert(io.open(source))
  local buffer = handle:read "*a"
  handle:close()

  local buffer = (buffer .. "\n$")
    :gsub("%-%-%[(%=*)%[.-%]%1%]", "")
    :gsub("%-%-[^\n]*", "")
    :gsub("[ \t]+\n", "\n")
    :gsub("\n\n+", "\n")
    :gsub("^\n+", "")

  local out = assert(io.open(result, "w"))
  out:write "return function (context) return {\n"
  for text, variable in buffer:gmatch "([^$]*)$([%w%_]*)" do
    local s = ""
    while text:find("%]" .. s .. "%]") do
      s = s .. "="
    end
    out:write("[", s, "[\n", text, "]", s, "];\n")
    if variable ~= "" then
      out:write(("context[%q];\n"):format(variable))
    end
  end
  out:write "} end\n"
  out:close()
end

build("dromozoa/parser/template.lua", "dromozoa/parser/runtime.lua")
build("dromozoa/regexp/template.lua", "dromozoa/regexp/runtime.lua")
