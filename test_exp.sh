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

for i in test/run/*.lua
do
  j=`expr "X$i" : 'X\(.*\)\.lua$'`
  case X$# in
    X0) lua "$i" >"$j.exp";;
    *) "$@" "$i" >"$j.exp";;
  esac
done
