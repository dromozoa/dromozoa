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

-- IEEE 754の16進数表記をなんとかする
-- Lua 5.2以降: tonumberが対応している
--   Cのstrtodが対応していない場合、Luaの互換実装を使う
--
-- https://qiita.com/mod_poppo/items/3fa4cdc35f9bfb352ad5
-- C#, JavaScriptは対応していない模様
--
-- 互換アルゴリズムとしては、
-- 1. strtodをそのまま実装する
-- 2. バイナリに変換して読みこむ
--    JavaScriptだったら型付き配列を使えばよさそう。どちらにせよ、あまり簡単に
--    ならないけど、ldexpがなくてもできる。形式に強く依存するという問題もあるか
--    （エンディアンも）。
--

local function hexadecimal_floating_numeral(s)
  -- 解析器から入ってきた場合、符号は持たないので簡略化する。
  -- 0x %x+        p
  --    %x+ %.
  --    %x+ %. %x+
  --        %. %x+
  --
  --
  --

  local dot = false
  local exponent = 0
  local significand = 0.0

  local i = 3
  while true do
    local byte = string.byte(s, i)
    if byte == nil then
      break
    elseif 0x30 <= byte and byte <= 0x39 then
      significand = significand * 16 + byte - 0x30
      exponent = exponent - 4
    elseif 0x41 <= byte and byte <= 0x46 then
      significand = significand * 16 + byte - 0x41 + 10
      exponent = exponent - 4
    elseif 0x61 <= byte and byte <= 0x66 then
      significand = significand * 16 + byte - 0x61 + 10
      exponent = exponent - 4
    elseif byte == 0x2E then -- '.'
      dot = true
      exponent = 0
    elseif byte == 0x50 or byte == 0x70 then -- '[Pp]'
      break
    else
      error("cannot parse near " .. string.sub(s, i, i))
    end
    i = i + 1
  end

  if not dot then
    exponent = 0
  end

  local sign = true
  local e = 0

  i = i + 1
  while true do
    local byte = string.byte(s, i)
    if byte == nil then
      break
    elseif byte == 0x2B then -- '+'
      -- noop
    elseif byte == 0x2D then -- '-'
      sign = false
    elseif 0x30 <= byte and byte <= 0x39 then
      e = e * 10 + byte - 0x30
    else
      error("cannot parse near " .. string.sub(s, i, i))
    end
    i = i + 1
  end
  exponent = exponent + (sign and e or -e)

  -- print(("0x%X, %d"):format(significand, exponent))
  local u = significand * math.pow(2, exponent)
  local v = math.ldexp(significand, exponent)
  assert(u == v)

  -- print(("%a"):format(v))
end

local s = ...
hexadecimal_floating_numeral(s or "0X1.921FB54442D18P+1")
