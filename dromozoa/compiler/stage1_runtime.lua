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
-- Under Section 7 of GPL version 3, you are granted additional
-- permissions described in the GCC Runtime Library Exception, version
-- 3.1, as published by the Free Software Foundation.
--
-- You should have received a copy of the GNU General Public License
-- and a copy of the GCC Runtime Library Exception along with
-- dromozoa.  If not, see <http://www.gnu.org/licenses/>.

local D = dromozoa
local G = globalThis

---------------------------------------------------------------------------

function error(message)
  D.error(message)
end

function assert(v, message, ...)
  if v then
    return v, message, ...
  elseif message == nil then
    return error "assertion failed!"
  else
    return error(message)
  end
end

function type(v)
  local t = D.typeof(v)
  if t == "undefined" then
    return "nil"
  elseif t == "number" then
    return "number"
  elseif t == "string" then
    return "string"
  elseif t == "boolean" then
    return "boolean"
  elseif t == "function" then
    return "function"
  end
  assert(t == "object")
  if D.instanceof(v, D.LuaFunction) then
    return "function"
  elseif D.instanceof(v, D.LuaTable) then
    return "table"
  else
    return "userdata"
  end
end

---------------------------------------------------------------------------

local string_metatable

D.getmetafield = D.export(function (object, event)
  local t = type(object)
  if t == "string" then
    return string_metatable[event]
  elseif t == "table" then
    local metatable = D.getmetatable(object)
    if metatable ~= nil then
      return metatable[event]
    end
  end
end)

function getmetatable(object)
  local t = type(object)
  if t == "string" then
    return string_metatable
  elseif t == "table" then
    local metatable = D.getmetatable(object)
    if metatable == nil or metatable.__metatable == nil then
      return metatable
    else
      return metatable.__metatable
    end
  end
end

function setmetatable(table, metatable)
  assert(type(table) == "table")
  local t = type(metatable)
  assert(t == "nil" or t == "table")
  assert(D.getmetafield(table, "__metatable") == nil, "cannot change a protected metatable")
  D.setmetatable(table, metatable)
  return table
end

---------------------------------------------------------------------------

local rawget = D.rawget
local rawset = D.rawset

D.OP_SETTABLE = D.export(function (t, k, v)
  if rawget(t, k) == nil then
    local metafield = D.getmetafield(t, "__newindex")
    if metafield ~= nil then
      if type(metafield) == "table" then
        metafield[k] = v
      else
        metafield(t, k, v)
      end
      return t
    end
  end
  return rawset(t, k, v)
end)

D.OP_GETTABLE = D.export(function (t, k)
  local v = rawget(t, k)
  if v == nil then
    local metafield = D.getmetafield(t, "__index")
    if metafield ~= nil then
      if type(metafield) == "table" then
        v = metafield[k]
      else
        v = metafield(t, k)
      end
    end
  end
  return v
end)

D.OP_CLOSE = D.export(function (object)
  if object ~= nil then
    D.getmetafield(object, "__close")(object)
  end
end)

---------------------------------------------------------------------------

local rawlen = D.rawlen

D.rawlen = D.export(function (v)
  local t = type(v)
  if t == "string" then
    return string.len(v)
  elseif t == "table" then
    return rawlen(v)
  elseif t == "userdata" then
    return v.length
  else
    return 0
  end
end)

---------------------------------------------------------------------------

function print(...)
  local result = table.pack(...)
  for i = 1, result.n do
    result[i] = tostring(result[i])
  end
  G.console:log(table.concat(result, "\t", 1, result.n))
end

function require(modname)
  if package.loaded == nil then
    package.loaded = {}
  end
  local module = package.loaded[modname]
  if module == nil then
    module = package.preload[modname]()
    package.loaded[modname] = module
  end
  return module
end

local function select_impl(index, v, ...)
  if index == 2 then
    return ...
  else
    return select_impl(index - 1, ...)
  end
end

function select(index, ...)
  if index == "#" then
    return D.select(...)
  elseif index == 1 then
    return ...
  else
    return select_impl(index, ...)
  end
end

function tostring(v)
  local t = type(v)
  if t == "nil" then
    return "nil"
  elseif t == "number" then
    return v:toString()
  elseif t == "string" then
    return v
  elseif t == "boolean" then
    return v and "true" or "false"
  elseif t == "table" then
    local metamethod = D.getmetafield(v, "__tostring")
    if metamethod ~= nil then
      local v = metamethod(v)
      local t = type(v)
      if t == "number" then
        return v:toString()
      end
      assert(t == "string")
      return v
    end
  end
  local metafield = D.getmetafield(v, "__name")
  if type(metafield) == "string" then
    t = metafield
  end
  return t .. ": " .. v
end

---------------------------------------------------------------------------

local function ipairs_impl(t, i)
  local i = i + 1
  local v = t[i]
  if v ~= nil then
    return i, v
  end
end

function ipairs(t)
  return ipairs_impl, t, 0
end

local function pairs_impl(iterator)
  local result = iterator:next()
  if not result.done then
    local value = result.value
    return value[0], value[1]
  end
end

function pairs(t)
  local metamethod = D.getmetafield(t, "__pairs")
  if metamethod ~= nil then
    local f, s, var = metamethod(t)
    return f, s, var
  else
    return pairs_impl, D.entries(t)
  end
end

---------------------------------------------------------------------------

local function table_unpack(list, i, j)
  if i < j then
    return list[i], table_unpack(list, i + 1, j)
  else
    return list[i]
  end
end

local table_sort_compare = D.export(function (a, b)
  if a < b then
    return -1
  elseif a > b then
    return 1
  else
    return 0
  end
end)

table = {
  concat = function (list, sep, i, j)
    if sep == nil then
      sep = ""
    end
    if i == nil then
      i = 1
    end
    if j == nil then
      j = #list
    end
    if i > j then
      return ""
    end

    local result = list[i]
    for i = i + 1, j do
      result = result .. sep .. list[i]
    end
    return result
  end;

  pack = function (...)
    return { n = select("#", ...), ... }
  end;

  unpack = function (list, i, j)
    if i == nil then
      i = 1
    end
    if j == nil then
      j = #list
    end
    return table_unpack(list, i, j)
  end;

  sort = function (list, comp)
    local array = D.newuserdata(G.Array, table.unpack(list));
    array:sort(comp == nil and table_sort_compare or D.export(function (a, b)
      if comp(a, b) then
        return -1
      elseif comp(b, a) then
        return 1
      else
        return 0
      end
    end))
    D.OP_SETLIST(list, array)
  end;
}

---------------------------------------------------------------------------

local string_decoder = D.newuserdata(G.TextDecoder)
local string_encoder = D.newuserdata(G.TextEncoder)

local function string_prepare(n, i, j)
  if i < 0 then
    i = i + n + 1
  end
  if i < 1 then
    i = 1
  end

  if j < 0 then
    j = j + n + 1
  end
  if j > n then
    j = n
  end

  return i, j
end

string = {
  len = function (s)
    local buffer = string_encoder:encode(s)
    return buffer.length
  end;

  byte = function (s, i, j)
    if i == nil then
      i = 1
    end
    if j == nil then
      j = i
    end
    local buffer = string_encoder:encode(s)
    local m, n = string_prepare(buffer.length, i, j)
    if m <= n then
      local n = n - m + 1
      local m = m - 2
      local result = {}
      for i = 1, n do
        result[i] = buffer[i + m]
      end
      return table.unpack(result, 1, n)
    end
  end;

  char = function (...)
    local source = table.pack(...)
    local buffer = D.newuserdata(G.Uint8Array, source.n)
    for i = 1, source.n do
      buffer[i - 1] = source[i]
    end
    return string_decoder:decode(buffer)
  end;

  sub = function (s, i, j)
    if j == nil then
      j = -1
    end
    local buffer = string_encoder:encode(s)
    local m, n = string_prepare(buffer.length, i, j)
    if m <= n then
      local n = n - m + 1
      local m = m - 2
      return string_decoder:decode(D.newuserdata(G.Uint8Array, buffer.buffer, m, n))
    else
      return ""
    end
  end;
}

string_metatable = {
  __index = string;
}

-- string
-- .sub
-- .char
-- .byte

--  :find
--  :format
--  :gmatch
--  :gsub
--  :len

-- .pack
-- .unpack

