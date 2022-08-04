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

local function stable_pairs(t, compare, n)
  local metatable = getmetatable(t)
  if metatable ~= nil then
    local metamethod = metatable["dromozoa.stable_pairs"]
    if metamethod ~= nil then
      return metamethod(t)
    end
    if metatable.__name == "dromozoa.array" then
      return t:ipairs()
    end
  end

  local K = {}
  for k in pairs(t) do
    K[#K + 1] = k
  end

  -- テーブルのインデックス列には
  -- 1. NaNは含まれない。
  -- 2. 等値なオブジェクトはたかだか1個含まれる。
  -- 3. 同値なオブジェクトは2個以上含まれるかもしれない。
  -- table.sortは同一要素を比較する場合がある。
  table.sort(K, function (a, b)
    -- 等値ならば同一要素の比較である。
    if rawequal(a, b) then
      return false
    end
    local c = compare(a, b, n)
    if c == 0 then
      -- 同値なオブジェクトの重複はエラーとする。
      error "table index is not unique"
    end
    return c < 0
  end)

  local i = 0
  return function (t)
    i = i + 1
    local k = K[i]
    return k, t[k]
  end, t, nil
end

local function compare(a, b, n)
  local typename = type(a)
  local t = typemap[typename]
  local u = typemap[type(b)]
  if t ~= u then
    return t < u and -1 or 1
  end

  if a == b then
    return 0
  end

  if t == 1 then
    return b and -1 or 1
  elseif t == 3 then
    -- NaNは数値と文字列のあいだに位置するものとして扱う。NaN同士は同値とみなす。
    local a_is_nan = a ~= a
    local b_is_nan = b ~= b
    if b_is_nan then
      return a_is_nan and 0 or -1
    elseif a_is_nan then
      return 1
    end
    return a < b and -1 or 1
  elseif t == 4 then
    return a < b and -1 or 1
  end

  local s, c = pcall(function ()
    if a < b then
      return -1
    elseif a > b then
      return 1
    else
      return 0
    end
  end)
  if s and c then
    return c
  end

  if t == 5 then
    n = n + 1
    if n > 2000 then
      error "too much recursion; possible loop detected"
    end

    local f, t, k, v = stable_pairs(b, compare, n)
    for j, u in stable_pairs(a, compare, n) do
      k, v = f(t, k)

      local c = compare(j, k, n)
      if c ~= 0 then
        return c
      end

      local c = compare(u, v, n)
      if c ~= 0 then
        return c
      end
    end

    k, v = f(t, k)
    return compare(nil, k, n)
  end

  error("attempt to compare two " .. typename .. " values")
end

return function (a, b)
  return compare(a, b, 0)
end
