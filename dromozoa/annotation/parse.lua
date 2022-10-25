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

local regexp = require "dromozoa.annotation.regexp"
local parser = require "dromozoa.annotation.parser"

local function process(u)
  local u_name = parser.symbol_names[u[0]]

  if u_name == "->" then
    local t1 = {}
    for i, v in ipairs(u[1]) do
      t1[i] = process(v)
    end
    local t2 = {}
    for i, v in ipairs(u[2]) do
      t2[i] = process(v)
    end
    return { t1, t2 }
  else
    return u_name
  end
end

return function(source)
  return process(regexp(source, source, parser.max_terminal_symbol, parser()))
end
