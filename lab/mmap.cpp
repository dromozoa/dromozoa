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
    mmapで確保したメモリにコードを書き込む。

    ELFやMACH-Oといったバイナリ形式を作成すればよいが、勉強するのが面倒なので、
    最初はmmapでやってみる。

    https://blog.miz-ar.info/2021/10/jit-on-aarch64/
 */

#include <sys/mman.h>

// https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man3/sys_icache_invalidate.3.html
#include <libkern/OSCacheControl.h>

#include <iostream>

int main(int ac, char* av[]) {
  size_t mmap_size = 4096;

  char* memory = static_cast<char*>(mmap(nullptr, mmap_size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0));
  if (memory == MAP_FAILED) {
    std::cerr << "mmap error\n";
    return 1;
  }

  // write code

  char* ptr = memory;

  int result = mprotect(memory, mmap_size, PROT_READ | PROT_EXEC);
  if (result != 0) {
    std::cerr << "mprotect error\n";
    return 1;
  }

  sys_icache_invalidate(memory, ptr - memory);

  munmap(memory, mmap_size);
  return 0;
}
