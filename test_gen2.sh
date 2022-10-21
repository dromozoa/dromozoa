#! /bin/sh -e

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

case X$1 in
  X) exec sh -e "$0" lua;;
esac

mkdir -p out/gen2

LUA_VERSION=`"$@" -e 'io.write(_VERSION)'`
if test "X$LUA_VERSION" = "XLua 5.4"
then
  for i in test/gen2/*.lua
  do
    j=`expr "X$i" : 'X\(.*\)\.lua$'`
    "$@" "$i" >"$j.exp"
  done
fi

for i in test/gen2/*.lua
do
  j=`expr "X$i" : 'Xtest/gen2/\([^/]*\)\.lua$'`
  "$@" test_gen2.lua "$i" >"out/gen2/$j.out"
  diff -u "test/gen2/$j.exp" "out/gen2/$j.out"
done

case X$DROMOZOA_TEST_DEBUG in
  X|X0) rm -fr out;;
esac
