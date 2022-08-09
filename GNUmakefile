# Copyright (C) 2022 Tomoyuki Fujimori <moyu@dromozoa.com>
#
# This file is part of dromozoa.
#
# dromozoa is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# dromozoa is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with dromozoa.  If not, see <http://www.gnu.org/licenses/>.

target = \
	dromozoa/regexp/runtime.lua \
	dromozoa/parser/runtime.lua \
	dromozoa/compiler/lua54_regexp.lua \
	dromozoa/compiler/lua54_parser.lua

all:: $(target)

check:: all
	./test.sh

dromozoa/regexp/runtime.lua: dromozoa/regexp/template.lua
	lua build_runtime.lua $< $@

dromozoa/parser/runtime.lua: dromozoa/parser/template.lua
	lua build_runtime.lua $< $@

dromozoa/compiler/lua54_regexp.lua dromozoa/compiler/lua54_parser.lua: build_lua54.lua
	lua $< dromozoa/compiler/lua54_regexp.lua dromozoa/compiler/lua54_parser.lua
