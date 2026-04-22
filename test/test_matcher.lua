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

local matcher = require "dromozoa.matcher"

local m = matcher.new([[
foobar
  bazqux
]], "test.txt")

assert(m.srcloc.position == 1)
assert(m.srcloc.line == 1)
assert(m.srcloc.column == 1)
assert(m._0 == nil)
assert(m._1 == nil)
assert(not m:eof())

assert(m:match "%a-%s+")
assert(m.srcloc.position == 10)
assert(m.srcloc.line == 2)
assert(m.srcloc.column == 3)
assert(m._0 == "foobar\n  ")
assert(m._1 == nil)
assert(not m:eof())

assert(m:match "%a(%a-)%a%s+")
assert(m.srcloc.position == 17)
assert(m.srcloc.line == 3)
assert(m.srcloc.column == 1)
assert(m._0 == "bazqux\n")
assert(m._1 == "azqu")
assert(m:eof())
