#! /bin/sh -e

# Copyright (C) 2020,2022 Tomoyuki Fujimori <moyu@dromozoa.com>
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

LUA_PATH="?.lua;ext/?.lua;;"
export LUA_PATH

for i in test/test*.lua
do
  "$@" "$i"
done

LUA_VERSION=`"$@" -e 'io.write(_VERSION)'`
if test "X$LUA_VERSION" = "XLua 5.4"
then
  for i in test/run/*.lua
  do
    j=`expr "X$i" : 'X\(.*\)\.lua$'`
    "$@" "$i" >"$j.exp"
  done
fi

for i in test/run/*.lua
do
  j=`expr "X$i" : 'Xtest/run/\([^/]*\)\.lua$'`
  "$@" compile_stage1.lua "test-$j.mjs" "test-$j.mjs.map" dromozoa/compiler/runtime_stage1.lua "$i"
  node --enable-source-maps "test-$j.mjs" >"test-$j.out"
  diff -u "test/run/$j.exp" "test-$j.out"
done

case X$DROMOZOA_TEST_DEBUG in
  X|X0) rm -f test*.dot test-gen*.lua test*.mjs test*.mjs.map test*.out;;
esac
