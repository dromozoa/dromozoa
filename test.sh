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

case X$1 in
  X) exec sh -e "$0" lua;;
esac

mkdir -p out/stage1

for i in test/test*.lua
do
  "$@" "$i"
done

LUA_VERSION=`"$@" -e 'io.write(_VERSION)'`
if test "X$LUA_VERSION" = "XLua 5.4"
then
  for i in test/stage1/*.lua
  do
    j=`expr "X$i" : 'X\(.*\)\.lua$'`
    "$@" "$i" >"$j.exp"
  done
fi

for i in test/stage1/*.lua
do
  j=`expr "X$i" : 'Xtest/stage1/\([^/]*\)\.lua$'`
  "$@" compile_stage1.lua "out/stage1/$j.mjs" "out/stage1/$j.mjs.map" dromozoa/compiler/stage1_runtime.lua "$i"
  node --enable-source-maps "out/stage1/$j.mjs" >"out/stage1/$j.out"
  diff -u "test/stage1/$j.exp" "out/stage1/$j.out"
done

case X$DROMOZOA_TEST_DEBUG in
  X|X0) rm -fr out;;
esac
