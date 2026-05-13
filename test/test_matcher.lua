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
local source_location = require "dromozoa.source_location"

--=========================================================================

local that = matcher.new([[
foobar
  bazqux
]], source_location.new "=(test)")

local srcloc = that.start_srcloc
assert(srcloc.position == 1)
assert(srcloc.line == 1)
assert(srcloc.column == 1)
assert(that._0 == nil)
assert(that._1 == nil)
assert(that:is_at_start())
assert(not that:is_at_end())

assert(that:match "%a-%s+")

local srcloc = that.start_srcloc
assert(srcloc.position == 10)
assert(srcloc.line == 2)
assert(srcloc.column == 3)
assert(that._0 == "foobar\n  ")
assert(that._1 == nil)
assert(not that:is_at_start())
assert(not that:is_at_end())

assert(that:match "%a(%a-)%a%s+")

local srcloc = that.start_srcloc
assert(srcloc.position == 17)
assert(srcloc.line == 3)
assert(srcloc.column == 1)
assert(that._0 == "bazqux\n")
assert(that._1 == "azqu")
assert(not that:is_at_start())
assert(that:is_at_end())

--=========================================================================

local source1 = [[
"foo \" bar \z
  baz\n"]]
local source2 = [=[
[====[
qux
]====]]=]
local that = matcher.new(source1 .. source2, source_location.new "=(test)")
assert(that:match_short_string())
assert(that._0 == source1)
assert(that._1 == "foo \" bar baz\n")
assert(that:match_long_string())
assert(that._0 == source2)
assert(that._1 == "qux\n")

--=========================================================================

local that = matcher.new([[
foobar
baz
qux
]], source_location.new("=(test)", 10, 2, 100))

local last_srcloc = that.last_srcloc
local start_srcloc = that.start_srcloc
assert(not last_srcloc)
assert(start_srcloc.position == 10)
assert(start_srcloc.line == 2)
assert(start_srcloc.column == 100)

-- 改行を含まないマッチ
assert(that:match "foo")
assert(not rawequal(last_srcloc, that.last_srcloc))
assert(not rawequal(start_srcloc, that.start_srcloc))
local last_srcloc = assert(that.last_srcloc)
local start_srcloc = that.start_srcloc
assert(last_srcloc.position == 12)
assert(last_srcloc.line == 2)
assert(last_srcloc.column == 102)
assert(start_srcloc.position == 13)
assert(start_srcloc.line == 2)
assert(start_srcloc.column == 103)

-- マッチしなかった場合、srclocは更新されない
assert(not that:match "foo")
assert(rawequal(last_srcloc, that.last_srcloc))
assert(rawequal(start_srcloc, that.start_srcloc))

-- 改行で終わるマッチ
assert(that:match "bar\n")
assert(not rawequal(last_srcloc, that.last_srcloc))
assert(not rawequal(start_srcloc, that.start_srcloc))
local last_srcloc = assert(that.last_srcloc)
local start_srcloc = that.start_srcloc
assert(last_srcloc.position == 16)
assert(last_srcloc.line == 2)
assert(last_srcloc.column == 106)
assert(start_srcloc.position == 17)
assert(start_srcloc.line == 3)
assert(start_srcloc.column == 1)

-- 改行を含むマッチ
assert(that:match "baz\nqux")
assert(not rawequal(last_srcloc, that.last_srcloc))
assert(not rawequal(start_srcloc, that.start_srcloc))
local last_srcloc = assert(that.last_srcloc)
local start_srcloc = that.start_srcloc
assert(last_srcloc.position == 23)
assert(last_srcloc.line == 4)
assert(last_srcloc.column == 3)
assert(start_srcloc.position == 24)
assert(start_srcloc.line == 4)
assert(start_srcloc.column == 4)

-- 改行だけのマッチ
assert(that:match "\n")
assert(not rawequal(last_srcloc, that.last_srcloc))
assert(not rawequal(start_srcloc, that.start_srcloc))
local last_srcloc = assert(that.last_srcloc)
local start_srcloc = that.start_srcloc
assert(last_srcloc.position == 24)
assert(last_srcloc.line == 4)
assert(last_srcloc.column == 4)
assert(start_srcloc.position == 25)
assert(start_srcloc.line == 5)
assert(start_srcloc.column == 1)

assert(not that:is_at_start())
assert(that:is_at_end())
