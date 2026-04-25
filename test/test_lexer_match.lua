-- Copyright (C) 2026 Tomoyuki Fujimori <moyu@dromozoa.com>
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
-- along with dromozoa.  If not, see <https://www.gnu.org/licenses/>.

local lexer = require "dromozoa.lexer"
local source_location = require "dromozoa.source_location"

local that = lexer.new()
that.source = [[
foobar
  bazqux
]]
that.srcloc = source_location.new "=test"

assert(that.srcloc.position == 1)
assert(that.srcloc.line == 1)
assert(that.srcloc.column == 1)
assert(that._0 == nil)
assert(that._1 == nil)
assert(not that:eof())

assert(that:match "%a-%s+")
assert(that.srcloc.position == 10)
assert(that.srcloc.line == 2)
assert(that.srcloc.column == 3)
assert(that._0 == "foobar\n  ")
assert(that._1 == nil)
assert(not that:eof())

assert(that:match "%a(%a-)%a%s+")
assert(that.srcloc.position == 17)
assert(that.srcloc.line == 3)
assert(that.srcloc.column == 1)
assert(that._0 == "bazqux\n")
assert(that._1 == "azqu")
assert(that:eof())
