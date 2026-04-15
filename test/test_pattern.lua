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

require "common-pattern"
require "common-string"

local range = pattern_range(char_class_new(), "AZaz__")
assert(char_class_test(range, string_byte("x", 1)))
assert(not char_class_test(range, string_byte("0", 1)))

local set = pattern_negate(pattern_set(char_class_new(), "02468"))
assert(char_class_test(set, string_byte("1", 1)))
assert(not char_class_test(set, string_byte("2", 1)))
