-- Copyright (C) 2025 Tomoyuki Fujimori <moyu@dromozoa.com>
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
-- along with dromozoa.  If not, see <https://www.gnu.org/licenses/>.

--
-- Luaの文法のままで静的型付けを実現できないか検討する
--
-- C++のGenericsとWITを参考にする
--

-- チャンクレベルは静的に実行され、型を定義できるというのはどうか
-- constexpr関数みたいなもの
-- 関数本体はいっしょに定義する
-- function name.fかname:fでレコードに関数を紐づける。

local string = record {
  size = u32;
  data = pointer(char);
}

local pair = class {
  key = string;
  value = string;
}

-- 再帰は？
local node = class {
  r = node;
  b = node;
}

local T = func(string, string):result(u32)

local T = func { string, string } { string }

-- 型の表現自体はできる
-- 型の指定はどうする？

function f(a, b)
  local c = a
  return a * b
end
T:of(f)

-- 型アノテーション


