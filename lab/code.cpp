// Copyright (C) 2022 Tomoyuki Fujimori <moyu@dromozoa.com>
//
// This file is part of dromozoa.
//
// dromozoa is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// dromozoa is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with dromozoa.  If not, see <http://www.gnu.org/licenses/>.

/*
  コンパイルしてobjdumpで見てみる。

  objdump -d code.o

0000000000000000 <__Z1fv>:
       0: 55                            pushq   %rbp
       1: 48 89 e5                      movq    %rsp, %rbp
       4: b8 2a 00 00 00                movl    $42, %eax
       9: 5d                            popq    %rbp
       a: c3                            retq

  https://docs.microsoft.com/ja-jp/cpp/build/x64-calling-convention?view=msvc-170

 */

#include <stdint.h>

int64_t f() {
  return 42;
}
