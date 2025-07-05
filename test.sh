#! /bin/sh -e

# Copyright (C) 2025 Tomoyuki Fujimori <moyu@dromozoa.com>
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
# along with dromozoa.  If not, see <https://www.gnu.org/licenses/>.

LUA_PATH="include-lua/?.lua;include/?.lua;;"
export LUA_PATH

wat2wasm="wasm-tools parse"
wasmtime="wasmtime run --dir=. -W gc"

lua boot.lua boot.lua | $wat2wasm -o test-boot.wasm -
lua boot.lua test.lua >test-stage0.wat
$wasmtime test-boot.wasm test.lua >test-stage1.wat
cmp test-stage0.wat test-stage1.wat
$wat2wasm test-stage1.wat >test-stage1.wasm

printf foobarbaz | lua test.lua foo bar baz >test-stage0.dat
printf foobarbaz | $wasmtime test-stage1.wasm foo bar baz >test-stage1.dat
cmp test-stage0.dat test-stage1.dat
