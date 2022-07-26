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

local tree_map = require "dromozoa.tree_map"

local typemap = {
  ["nil"]      = 0; -- LUA_TNIL
  ["boolean"]  = 1; -- LUA_TBOOLEAN
                    -- LUA_TLIGHTUSERDATA
  ["number"]   = 3; -- LUA_TNUMBER
  ["string"]   = 4; -- LUA_TSTRING
  ["table"]    = 5; -- LUA_TTABLE
  ["function"] = 6; -- LUA_TFUNCTION
  ["userdata"] = 7; -- LUA_TUSERDATA
  ["thread"]   = 8; -- LUA_TTHREAD
}

local function compare(a, b)
  -- メタメソッドが呼ばれるかもしれない
  if a == b then
    return 0
  end
  -- エラーになるかもしれないけど呼んでみる？
  -- local result
  -- pcall(function () result = a < b end)

  local s = typemap[type(a)]
  local t = typemap[type(b)]
  if s ~= t then
    return s < t and -1 or 1
  end

  assert(t ~= 0)
  if t == 1 then
    return b and -1 or 1
  elseif t == 3 then
    return a < b and -1 or 1
  elseif t == 4 then
    return a < b and -1 or 1
  elseif t == 5 then
    -- stableだったら、巡回して、辞書順比較する

    -- stableじゃなかったら？
    -- stableなキーリストを作る
    -- 問題点として、おなじ構造のテーブルがキーになってると詰む
    -- というか、ソートするためにcompareが必要になる

    local m = getmetatable(a)
    local n = getmetatable(b)

    -- 両方ともstable
    if m ~= nil and m == n and m.__name == "dromozoa.tree_map" then
      local f, t, k, v = pairs(b)
      for j, u in pairs(a) do
        k, v = f(t, k)

        local c = compare(j, k)
        if c ~= 0 then
          return c
        end

        local c = compare(u, v)
        if c ~= 0 then
          return c
        end

        assert(k ~= nil)
      end

      k, v = f(t, k)
      return compare(nil, k)
    else
      local K = {}
      for k in pairs(a) do
        assert(typemap[type(k)] < 5)
        K[#K + 1] = k
      end
      for k in pairs(b) do
        assert(typemap[type(k)] < 5)
        if a[k] == nil then
          K[#K + 1] = k
        end
      end
      table.sort(K, function (a, b) return compare(a, b) < 0 end)

      for _, k in ipairs(K) do
        local c = compare(a[k], b[k])
        if c ~= 0 then
          return c
        end
      end

      return 0
    end

  else
    -- サポートしない
    -- 文字列に変換してポインタを拾う？
    error "not supported"
  end
end

local t1 = tree_map(compare)
t1[1] = 42
t1[2] = 69

local t2 = tree_map(compare)
t2(1)(1)(1)(1)[1] = 42

print(compare({abc=42}, {abz=42,abc=nil}))

