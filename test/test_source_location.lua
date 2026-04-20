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

local source_location = require "dromozoa.source_location"

local srcloc = source_location.new ""

assert(srcloc.position == 1)
assert(srcloc.line == 1)
assert(srcloc.column == 1)

srcloc:update "foo"

assert(srcloc.position == 4)
assert(srcloc.line == 1)
assert(srcloc.column == 4)

local srcloc2 = srcloc:clone()
srcloc:update "bar\n"

assert(srcloc.position == 8)
assert(srcloc.line == 2)
assert(srcloc.column == 1)

assert(srcloc2.position == 4)
assert(srcloc2.line == 1)
assert(srcloc2.column == 4)

srcloc:update "baz\nqux"

assert(srcloc.position == 15)
assert(srcloc.line == 3)
assert(srcloc.column == 4)
