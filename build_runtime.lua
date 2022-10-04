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

local function build(source, result)
  local handle = assert(io.open(source))
  local buffer = handle:read "*a"
  handle:close()

  local buffer = (buffer .. "$")
    :gsub("%-%-%[(%=*)%[.-%]%1%]", "")
    :gsub("%-%-[^\n]*", "")
    :gsub("[ \t]+\n", "\n")
    :gsub("\n\n+", "\n")
    :gsub("^\n+", "")

  local _1, _2
  function match(s, pattern)
    _1, _2 = s:match(pattern)
    return _1 ~= nil
  end

  local out = assert(io.open(result, "w"))
  out:write "return function (context) return {\n"

  while true do
    local text
    text, buffer = assert(buffer:match "(.-)($.*)")

    if text ~= "" then
      local s = ""
      while text:find("%]" .. s .. "%]") do
        s = s .. "="
      end
      out:write("[", s, "[\n", text, "]", s, "];\n")
    end

    if buffer == "$" then
      break
    end

    assert(match(buffer, "^$([%a_][%w_]*)(.*)") or match(buffer, "^${'(..-)'}(.*)"))
    out:write("context[", quote_lua(_1), "];\n")
    buffer = _2
  end

  out:write "} end\n"
  out:close()
end

build(...)
