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

local srcloc1 = source_location.new "=(test)"

assert(srcloc1.filename == "=(test)")
assert(srcloc1.position == 1)
assert(srcloc1.line == 1)
assert(srcloc1.column == 1)

assert(srcloc1:to_string() == "=(test):1:1")
assert(source_location.to_string(nil) == "=(unknown):0:0")

local srcloc2 = source_location.new("=(test)", 2, 1, 2)
local srcloc3 = source_location.new "=(zzzz)"

assert(srcloc1:compare(srcloc1) == 0)
assert(srcloc1:compare(srcloc2) < 0)
assert(srcloc2:compare(srcloc1) > 0)
assert(srcloc1:compare(srcloc3) < 0)
assert(srcloc2:compare(srcloc3) < 0)
