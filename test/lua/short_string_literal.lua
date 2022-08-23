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

print("foo\
bar\z
  baz\zqux","\a\f\n\r\t\v",
"\33\9\034",
-- "\999",
"\x4B\x4b",
"\u{3042}",
"\u{7FFFFFFF}",
-- "\u{80000000}",
"\u{000000000000003042}",
"\u{00000000007FFFFFFF}",
-- "\u{0000000080000000}",
-- "\u{800000000000003042}",
""
)
