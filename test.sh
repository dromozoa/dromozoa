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

mkdir -p out/gen1

for i in test/test*.lua
do
  "$@" "$i"
done

LUA_VERSION=`"$@" -e 'io.write(_VERSION)'`
if test "X$LUA_VERSION" = "XLua 5.4"
then
  for i in test/gen1/*.lua
  do
    j=`expr "X$i" : 'X\(.*\)\.lua$'`
    "$@" "$i" foo 42 "bar baz qux" >"$j.exp"
  done
fi

for i in test/gen1/*.lua
do
  j=`expr "X$i" : 'Xtest/gen1/\([^/]*\)\.lua$'`
  "$@" tool/compile_gen1.lua "out/gen1/$j.mjs" dromozoa/compiler/gen1_runtime.lua "$i"
  node "out/gen1/$j.mjs" foo 42 "bar baz qux" >"out/gen1/$j.out"
  diff -u "test/gen1/$j.exp" "out/gen1/$j.out"
done

time "$@" tool/compile_gen1.lua out/gen1_stg1.mjs dromozoa/compiler/gen1_runtime.lua tool/compile_gen1.lua
time node out/gen1_stg1.mjs out/gen1_stg2.mjs dromozoa/compiler/gen1_runtime.lua tool/compile_gen1.lua
diff -u out/gen1_stg1.mjs out/gen1_stg2.mjs

case X$DROMOZOA_TEST_DEBUG in
  X|X0) rm -fr out;;
esac
