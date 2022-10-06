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

local quote_js = require "dromozoa.quote_js"

local s = [====[
fcall(1)
-- foo
-- bar
-- baz
--[[ ]=] ]]
--[==[ ]=] ]==]
_fcall(0)fcall(2)fcall_(0)fcall(3)
]====]

s = s
  :gsub("%-%-%[(%=*)%[.-%]%1%]", "")
  :gsub("%-%-[^\n]*", "")
  :gsub("[ \t]+\n", "\n")
  :gsub("\n\n+", "\n")
  :gsub("^\n+", "")

print(s)
print(quote_js("\0\a\b\t\n\v\f\r\"\\\127\u{2028}\u{2029}"))

local s = "abcdeabcde"
print(s:gsub("[a-d]", "%1"))
print(s:gsub("a(bc)d", "%1"))
print(s:gsub("[a-d]", { a = "A", b = 42, c = false }))
print(s:gsub("[a-d]([a-d])", { a = "A", b = 42, c = false }))
