/*
  Copyright (C) 2022 Tomoyuki Fujimori <moyu@dromozoa.com>

  This file is part of dromozoa.

  dromozoa is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  dromozoa is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with dromozoa.  If not, see <http://www.gnu.org/licenses/>.

  https://github.com/jstrieb/small-elf

*/

#include <stdint.h>

int32_t add(int32_t, int32_t);
int32_t sub(int32_t, int32_t);
int32_t mul(int32_t, int32_t);
int32_t div(int32_t, int32_t);
int32_t mod(int32_t, int32_t);
void print(int32_t);

int main(int ac, char* av[]) {
  int32_t u = add(42, 69);
  int32_t v = div(u, 3);
  print(v); /* 37 */
  return 0;
}
