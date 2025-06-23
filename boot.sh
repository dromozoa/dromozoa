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

wasm1="wasmer run --dir=."
wasm2="wasmtime run --dir=."

cat boot-rt1.lua boot.lua >boot-merged.dat
lua -l boot-rt0 boot.lua boot-merged.dat >boot-stage0.wat
wat2wasm boot-stage0.wat
$wasm1 boot-stage0.wasm boot-merged.dat >boot-stage1.wat
$wasm2 boot-stage0.wasm boot-merged.dat >boot-stage2.wat
cmp boot-stage0.wat boot-stage1.wat
cmp boot-stage0.wat boot-stage2.wat
