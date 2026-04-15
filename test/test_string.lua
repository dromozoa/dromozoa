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

require "common-string"

local s = "foobarbaz"
assert(string_compare(string_sub(s, 4, 6), "bar") == 0)
assert(string_compare("bar", "baz") < 0)
assert(string_compare("foo", "foobar") < 0)
assert(string_compare("barbaz", "bar") > 0)

assert(string_starts_with("foobarbaz", "bar", 4))
assert(not string_starts_with("foobarbaz", "baz", 4))
assert(not string_starts_with("foobar", "barbaz", 4))
assert(not string_starts_with("foobar", "baz", 4))

assert(string_find("foobarbaz", "foo", 1) == 1)
assert(string_find("foobarbaz", "foo", 4) == 0)
assert(string_find("foobarbaz", "foo", 7) == 0)

assert(string_find("foobarbaz", "bar", 1) == 4)
assert(string_find("foobarbaz", "bar", 4) == 4)
assert(string_find("foobarbaz", "bar", 7) == 0)

assert(string_find("foobarbaz", "baz", 1) == 7)
assert(string_find("foobarbaz", "baz", 4) == 7)
assert(string_find("foobarbaz", "baz", 7) == 7)

assert(string_find("foobarbaz", "qux", 1) == 0)
assert(string_find("foobarbaz", "qux", 4) == 0)
assert(string_find("foobarbaz", "qux", 7) == 0)
