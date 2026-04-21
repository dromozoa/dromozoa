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

local filename = ...
local handle = assert(io.open(filename, "wb"))

handle:write(
  "",
  "\a\b\f\n\r\t\v\\\"\'",
  '\a\b\f\n\r\t\v\\\"\'',
  "a\
b\zc\z d\z
e\z
 f",
  "\x00\xfe\xeD\xFa\xCE",
  "\0\01\0234",
  "\u{41}\u{00000041}\u{0000000000000041}",
  "\u{2262}\u{0391}\u{002e}",
  "\u{D55C}\u{AD6D}\u{C5B4}",
  "\u{65e5}\u{672c}\u{8a9e}",
  "\u{FEFF}\u{0233B4}",
  [[foo
bar]],
  [[
foo
bar
]],
  [=[]]]=]
)

handle:close()
