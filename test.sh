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
  case X$# in
    X0) lua "$i";;
    *) "$@" "$i";;
  esac
done

case X$# in
  X0) LUA_VERSION=`lua -e 'io.write(_VERSION)'`;;
  *) LUA_VERSION=`"$@" -e 'io.write(_VERSION)'`;;
esac
if test "X$LUA_VERSION" = "XLua 5.4"
then
  ./test_exp.sh "$@"
fi

for i in test/run/*.lua
do
  j=`expr "X$i" : 'Xtest/run/\([^/]*\)\.lua$'`
  case X$# in
    X0) lua test/run_js.lua "$i" "test-$j.js";;
    *) "$@" test/run_js.lua "$i" "test-$j.js";;
  esac
  node "test-$j.js" >"test-$j.out"
  diff -u "test/run/$j.exp" "test-$j.out"
done

case X$DROMOZOA_TEST_DEBUG in
  X|X0) rm -f test*.dot test-gen*.lua test*.js test*.out;;
esac
