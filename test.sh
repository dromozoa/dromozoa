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

wasm=wasmer

cat boot-rt1.lua boot.lua | lua -l boot-rt0 boot.lua | wat2wasm -o test-boot.wasm -
cat boot-rt1.lua test.lua | lua -l boot-rt0 boot.lua >test-stage1.wat
cat boot-rt1.lua test.lua | $wasm run test-boot.wasm >test-stage2.wat
wat2wasm test-stage1.wat
wat2wasm test-stage2.wat
printf foobarbaz | lua -l boot-rt0 test.lua >test-stage0.dat
printf foobarbaz | $wasm run test-stage1.wasm >test-stage1.dat
printf foobarbaz | $wasm run test-stage2.wasm >test-stage2.dat
cmp test-stage1.wasm test-stage2.wasm
cmp test-stage0.dat test-stage1.dat
cmp test-stage0.dat test-stage2.dat
