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

LUA_PATH="include0/?.lua;;"
export LUA_PATH

wasm1="wasmer run --dir=."
wasm2="wasmtime run --dir=."

cat include1/runtime.lua boot.lua >boot-merged.dat
lua -l runtime boot.lua boot-merged.dat | wat2wasm -o test-boot.wasm -

cat include1/runtime.lua test.lua >test-merged.dat
lua -l runtime boot.lua test-merged.dat >test-stage0.wat
$wasm1 test-boot.wasm test-merged.dat >test-stage1.wat
$wasm2 test-boot.wasm test-merged.dat >test-stage2.wat
wat2wasm test-stage0.wat
wat2wasm test-stage1.wat
wat2wasm test-stage2.wat
cmp test-stage0.wat test-stage1.wat
cmp test-stage0.wat test-stage2.wat

printf foobarbaz | lua -l runtime test.lua foo bar baz >test-stage0.dat
printf foobarbaz | $wasm1 test-stage1.wasm foo bar baz >test-stage1.dat
printf foobarbaz | $wasm2 test-stage2.wasm foo bar baz >test-stage2.dat
cmp test-stage0.dat test-stage1.dat
cmp test-stage0.dat test-stage2.dat
