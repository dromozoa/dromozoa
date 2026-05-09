#! /bin/sh

# Copyright (C) 2026 Tomoyuki Fujimori <moyu@dromozoa.com>
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

separator=================================================================================
t() {
  printf '%s\ncode: %s\n' "$separator" "$1"
  lua -e "$1"
}

t 'local x = ='
t 'x = { f = function () end } x:f 42'
t 'function f() end f() = 42'
