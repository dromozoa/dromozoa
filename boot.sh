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

wasm1="wasmer run --dir=."
wasm2="wasmtime run --dir=."

lua boot.lua boot.lua >boot-stage0.wat
wat2wasm boot-stage0.wat
$wasm1 boot-stage0.wasm boot.lua >boot-stage1.wat
$wasm2 boot-stage0.wasm boot.lua >boot-stage2.wat
cmp boot-stage0.wat boot-stage1.wat
cmp boot-stage0.wat boot-stage2.wat
