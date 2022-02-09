-- Copyright (C) 2022 Tomoyuki Fujimori <moyu@dromozoa.com>
--
-- This file is part of dromozoa.
--
-- dromozoa is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- dromozoa is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with dromozoa.  If not, see <http://www.gnu.org/licenses/>.

-- ここからのコメントはmach-oについてのもの

-- https://github.com/aidansteele/osx-abi-macho-file-format-reference
-- https://developers.wonderpla.net/entry/2021/03/19/105503

-- 読みたいんだったら、otool使えばよいのでは？
-- #include <mach-o/fat.h>
-- #include <mach-o/loader.h>

-- https://stackoverflow.com/questions/32453849/minimal-mach-o-64-binary/32659692

-- int main(int ac, char* av[]) {
--   char* data = nullptr;
--   return 0;
-- }

-- 目的はバイナリを作成すること？
-- 実行可能ファイル？
-- 共有リンクファイル？

-- まずはなにかを出力する実行可能ファイルを作ってみる
-- リンカを作ること？

-- libbfdは？
-- 抽象化はあとまわし

-- ここからのコメントが.dataセクション生成のため

local data = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
local n = #data
local m = 16

io.write [[
#include <stdint.h>
]]

io.write "\n"

io.write(("static const int32_t SIZE = %d;\n"):format(n))

io.write "\n"

io.write "static const char CDATA[] =\n"
for i = 1, n, m do
  io.write(("  \"%s\"\n"):format(data:sub(i, i + m - 1)))
end
io.write ";\n"

io.write "\n"

io.write "static const int32_t IDATA[] = {\n"
for i = 1, n, m do
  local s = data:sub(i, i + m - 1)
  io.write " "
  for j = 1, #s, 4 do
    local a, b, c, d = s:byte(j, j + 3)
    io.write((" 0x%02X%02X%02X%02X,"):format(d or 0, c or 0, b or 0, a))
    -- io.write((" \"%-4s\""):format(s:sub(j, j + 3)))
  end
  io.write "\n"
end
io.write "};\n"


