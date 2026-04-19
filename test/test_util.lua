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

local util = require "dromozoa.util"

assert(util.normalize_eol "foo\n\n\nbar\n" == "foo\n\n\nbar\n")
assert(util.normalize_eol "foo\r\r\rbar\r" == "foo\n\n\nbar\n")
assert(util.normalize_eol "foo\r\n\r\n\r\nbar\r\n" == "foo\n\n\nbar\n")
assert(util.normalize_eol "foo\n\r\n\r\n\rbar\n\r" == "foo\n\n\nbar\n")

util.read_file(arg[0])
