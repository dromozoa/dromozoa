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
--
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

local path = ...

local handle = assert(io.open(path, "rb"))
local data = handle:read "*a"
handle:close()
