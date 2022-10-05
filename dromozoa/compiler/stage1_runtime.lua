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

local D_LuaFunction = D.LuaFunction
local D_LuaTable = D.LuaTable
local D_typeof = D.typeof
local D_instanceof = D.instanceof
local D_error = D.error
local D_getmetatable = D.getmetatable
local D_setmetatable = D.setmetatable
local D_rawset = D.rawset
local D_rawget = D.rawget
local D_rawlen = D.rawlen
local D_export = D.export
local D_select = D.select
local D_newuserdata = D.newuserdata
local D_entries = D.entries
local D_replace = D.replace

local string_metatable
local string_len

---------------------------------------------------------------------------

local function type(v)
  local t = D_typeof(v)
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
  if D_instanceof(v, D_LuaFunction) then
    return "function"
  elseif D_instanceof(v, D_LuaTable) then
    return "table"
  else
    return "userdata"
  end
end

local function error(message)
  D_error(message)
end

local function assert(...)
  if ... then
    return ...
  else
    local _, message = ...
    if message == nil then
      return D_error "assertion failed!"
    else
      return D_error(message)
    end
  end
end

local function getmetafield(object, event)
  local t = type(object)
  if t == "string" then
    return D_rawget(string_metatable, event)
  elseif t == "table" then
    local metatable = D_getmetatable(object)
    if metatable ~= nil then
      return D_rawget(metatable, event)
    end
  end
end

local function getmetatable(object)
  local t = type(object)
  if t == "string" then
    return string_metatable
  elseif t == "table" then
    local metatable = D_getmetatable(object)
    if metatable == nil or metatable.__metatable == nil then
      return metatable
    else
      return metatable.__metatable
    end
  end
end

local function setmetatable(table, metatable)
  assert(type(table) == "table")
  local t = type(metatable)
  assert(t == "nil" or t == "table")
  assert(getmetafield(table, "__metatable") == nil, "cannot change a protected metatable")
  D_setmetatable(table, metatable)
  return table
end

local function rawlen(v)
  local t = type(v)
  if t == "string" then
    return string_len(v)
  elseif t == "table" then
    return D_rawlen(v)
  elseif t == "userdata" then
    return v.length
  else
    return 0
  end
end

local function OP_SETTABLE(object, k, v)
  local t = type(object)
  assert(t == "table" or t == "userdata")
  if D_rawget(object, k) == nil then
    local metafield = getmetafield(object, "__newindex")
    if metafield ~= nil then
      if type(metafield) == "table" then
        metafield[k] = v
      else
        metafield(object, k, v)
      end
      return object
    end
  end
  return D_rawset(object, k, v)
end

local function OP_GETTABLE(object, k)
  local t = type(object)
  if t == "table" or t == "userdata" then
    local v = D_rawget(object, k)
    if v ~= nil then
      return v
    end
  end
  local metafield = getmetafield(object, "__index")
  if metafield ~= nil then
    if type(metafield) == "table" then
      return metafield[k]
    else
      return metafield(object, k)
    end
  end
end

local function OP_CLOSE(object)
  if object ~= nil then
    getmetafield(object, "__close")(object)
  end
end

---------------------------------------------------------------------------

_ENV.type = type
_ENV.error = error
_ENV.assert = assert
_ENV.getmetatable = getmetatable
_ENV.setmetatable = setmetatable
D.getmetafield = D_export(getmetafield)
D.rawlen = D_export(rawlen)
D.OP_SETTABLE = D_export(OP_SETTABLE)
D.OP_GETTABLE = D_export(OP_GETTABLE)
D.OP_CLOSE = D_export(OP_CLOSE)

---------------------------------------------------------------------------

local function select_impl(index, v, ...)
  if index == 2 then
    return ...
  else
    return select_impl(index - 1, ...)
  end
end

local function select(index, ...)
  if index == "#" then
    return D_select(...)
  elseif index == 1 then
    return ...
  else
    return select_impl(index, ...)
  end
end

local function table_pack(...)
  return { n = D_select(...), ... }
end

local function table_unpack_impl(list, i, j)
  if i == j then
    return list[i]
  else
    return list[i], table_unpack_impl(list, i + 1, j)
  end
end

local function table_unpack(list, i, j)
  if i == nil then
    i = 1
  end
  if j == nil then
    j = #list
  end
  if i > j then
    return
  end

  return table_unpack_impl(list, i, j)
end

local function table_concat(list, sep, i, j)
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
end

local function table_sort(list, comp)
  if comp == nil then
    comp = function (a, b) return a < b end
  end

  local n = #list
  local array = D_newuserdata(G.Array, table_unpack(list, 1, n));
  array:sort(D_export(function (a, b)
    if comp(a, b) then
      return -1
    elseif comp(b, a) then
      return 1
    else
      return 0
    end
  end))
  for i = 1, n do
    list[i] = array[i - 1]
  end
end

local table = {
  concat = table_concat;
  pack = table_pack;
  unpack = table_unpack;
  sort = table_sort;
}

_ENV.select = select
_ENV.table = table

---------------------------------------------------------------------------

local function tostring(v)
  local t = type(v)
  if t == "nil" then
    return "nil"
  elseif t == "number" then
    return G:String(v)
  elseif t == "string" then
    return v
  elseif t == "boolean" then
    return v and "true" or "false"
  elseif t == "table" then
    local metafield = getmetafield(v, "__tostring")
    if metafield ~= nil then
      local v = metafield(v)
      local t = type(v)
      if t == "number" then
        return G:String(v)
      end
      assert(t == "string")
      return v
    end
  end
  local metafield = getmetafield(v, "__name")
  if type(metafield) == "string" then
    t = metafield
  end
  return t .. ": " .. v
end

local function print(...)
  local result = table_pack(...)
  for i = 1, result.n do
    result[i] = tostring(result[i])
  end
  G.console:log(table_concat(result, "\t", 1, result.n))
end

local function require(modname)
  local module = package.loaded[modname]
  if module == nil then
    module = package.preload[modname]()
    package.loaded[modname] = module
  end
  return module
end

local function ipairs_impl(t, i)
  local i = i + 1
  local v = t[i]
  if v ~= nil then
    return i, v
  end
end

local function ipairs(t)
  return ipairs_impl, t, 0
end

local function pairs_impl(iterator)
  local result = iterator:next()
  if not result.done then
    local value = result.value
    return value[0], value[1]
  end
end

local function pairs(t)
  local metamethod = getmetafield(t, "__pairs")
  if metamethod ~= nil then
    local f, s, var = metamethod(t)
    return f, s, var
  else
    return pairs_impl, D_entries(t)
  end
end

_ENV.tostring = tostring
_ENV.print = print
_ENV.require = require
_ENV.ipairs = ipairs
_ENV.pairs = pairs

---------------------------------------------------------------------------

local string_decoder = D_newuserdata(G.TextDecoder)
local string_encoder = D_newuserdata(G.TextEncoder)

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

local RE1 = D_newuserdata(G.RegExp, [[\\]], "gs")
local RE2 = D_newuserdata(G.RegExp, [[\%z]], "gs")
local RE3 = D_newuserdata(G.RegExp, [[\%(.)]], "gs")
local RE4 = D_newuserdata(G.RegExp, [[\.\-]], "gs")

local function string_pattern(s)
  s = D_replace(s, RE1, [[\\]])
  s = D_replace(s, RE2, [[\u0000]])
  s = D_replace(s, RE3, [[\$1]])
  s = D_replace(s, RE4, [[.*?]])
  return s
end

local RE1 = D_newuserdata(G.RegExp, [[\$]], "gs")
local RE2 = D_newuserdata(G.RegExp, [[\%0]], "gs")
local RE3 = D_newuserdata(G.RegExp, [[\%([1-9])]], "gs")
local RE4 = D_newuserdata(G.RegExp, [[\%\%]], "gs")

local function string_replace(s)
  s = D_replace(s, RE1, [[$$$$]])
  s = D_replace(s, RE2, [[$$&]])
  s = D_replace(s, RE3, [[$$$1]])
  s = D_replace(s, RE4, [[%]])
  return s
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
      return table_unpack(result, 1, n)
    end
  end;

  char = function (...)
    local source = table_pack(...)
    local buffer = D_newuserdata(G.Uint8Array, source.n)
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
      return string_decoder:decode(D_newuserdata(G.Uint8Array, buffer.buffer, m - 1, n - m + 1))
    else
      return ""
    end
  end;

  gsub = function (s, pattern, repl)
    local re = D_newuserdata(G.RegExp, string_pattern(pattern), "gs")
    local t = type(repl)
    if t == "string" then
      return D_replace(s, re, string_replace(repl))
    elseif t == "table" then
      return D_replace(s, re, D_export(function (a, b)
        local v
        if type(b) == "number" then
          v = repl[a]
        else
          v = repl[b]
        end
        if v then
          return v
        else
          return a
        end
      end))
    end
    assert(t == "function")
    return D_replace(s, re, D_export(function (a, b, ...)
      local v
      if type(b) == "number" then
        v = repl(a)
      else
        v = repl(b, ...)
      end
      if v then
        return v
      else
        return a
      end
    end))
  end;
}

string_len = string.len

string_metatable = {
  __index = string;
}
