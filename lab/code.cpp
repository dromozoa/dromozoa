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

return 42
0000000000000000 <__Z1fv>:
       0: 55                            pushq   %rbp
       1: 48 89 e5                      movq    %rsp, %rbp
       4: b8 2a 00 00 00                movl    $42, %eax
       9: 5d                            popq    %rbp
       a: c3                            retq

return a + b
0000000000000000 <__Z1fxx>:
       0: 55                            pushq   %rbp
       1: 48 89 e5                      movq    %rsp, %rbp
       4: 48 8d 04 37                   leaq    (%rdi,%rsi), %rax
       8: 5d                            popq    %rbp
       9: c3                            retq

  https://docs.microsoft.com/ja-jp/cpp/build/x64-calling-convention?view=msvc-170

 */

#include <stdint.h>

int64_t f(int64_t a, int64_t b) {
  return a + b;
}
