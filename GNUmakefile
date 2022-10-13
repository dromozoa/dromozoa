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
	dromozoa/compiler/lua54_parser.lua \
	dromozoa/compiler/stage1_preamble.lua

all:: $(target)

check:: all
	./test.sh lua

dromozoa/regexp/runtime.lua: tool/template_lua.lua dromozoa/regexp/runtime.tmpl
	lua tool/template_lua.lua dromozoa/regexp/runtime.tmpl $@

dromozoa/parser/runtime.lua: tool/template_lua.lua dromozoa/parser/runtime.tmpl
	lua tool/template_lua.lua dromozoa/parser/runtime.tmpl $@

dromozoa/compiler/lua54_regexp.lua dromozoa/compiler/lua54_parser.lua: tool/generate_lua54.lua dromozoa/regexp/runtime.lua dromozoa/parser/runtime.lua
	lua tool/generate_lua54.lua dromozoa/compiler/lua54_regexp.lua dromozoa/compiler/lua54_parser.lua

dromozoa/compiler/stage1_preamble.lua: tool/template_js.lua dromozoa/compiler/stage1_preamble.tmpl
	lua tool/template_js.lua dromozoa/compiler/stage1_preamble.tmpl $@
