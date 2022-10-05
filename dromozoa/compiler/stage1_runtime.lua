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
    return "userdata"
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

local string_encoder = D_newuserdata(G.TextEncoder)
local string_decoder = D_newuserdata(G.TextDecoder)

local function string_encode_utf8(s)
  return string_encoder:encode(s)
end

local function string_decode_utf8(b)
  return string_decoder:decode(b)
end

function string_len(s)
  return string_encode_utf8(s).length
end

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

local function string_byte(s, i, j)
  local buffer = string_encode_utf8(s)
  if i == nil then
    i = 1
  end
  if j == nil then
    j = i
  end
  local m, n = string_prepare(buffer.length, i, j)
  if m > n then
    return
  end

  local n = n - m + 1
  local m = m - 2
  local result = {}
  for i = 1, n do
    result[i] = buffer[i + m]
  end
  return table_unpack(result, 1, n)
end

local function string_char(...)
  return string_decode_utf8(G.Uint8Array:from(D_newuserdata(G.Array, ...)))
end

local function string_sub(s, i, j)
  local buffer = string_encode_utf8(s)
  if j == nil then
    j = -1
  end
  local m, n = string_prepare(buffer.length, i, j)
  if m > n then
    return ""
  end

  return string_decode_utf8(D_newuserdata(G.Uint8Array, buffer.buffer, m - 1, n - m + 1))
end

local function string_gsub_regexp(pattern)
  return D_newuserdata(G.RegExp, pattern, "gs")
end

local RE1 = string_gsub_regexp [[\\]]
local RE2 = string_gsub_regexp [[\%z]]
local RE3 = string_gsub_regexp [[\%(.)]]
local RE4 = string_gsub_regexp [[\.\-]]

local function string_gsub_pattern(s)
  s = D_replace(s, RE1, [[\\]])
  s = D_replace(s, RE2, [[\u0000]])
  s = D_replace(s, RE3, [[\$1]])
  s = D_replace(s, RE4, [[.*?]])
  return s
end

local RE = string_gsub_regexp [[\%([\%0-9])]]

local function string_gsub_string(repl, match, p)
  return D_replace(repl, RE, D_export(function (a, b)
    if b == "%" then
      return "%"
    elseif b == "0" then
      return match
    else
      local v = p[G.Number(b)]
      if v == nil then
        return a
      else
        return v
      end
    end
  end))
end

local function string_gsub(s, pattern, repl)
  local replacer
  local t = type(repl)
  if t == "string" then
    replacer = function (match, p)
      return string_gsub_string(repl, match, p)
    end
  elseif t == "table" then
    replacer = function (match, p)
      return repl[p[1]]
    end
  else
    replacer = function (match, p)
      return repl(table_unpack(p, 1, p.n))
    end
  end

  local re = string_gsub_regexp(string_gsub_pattern(pattern))
  local n = 0
  local result = D_replace(s, re, D_export(function (match, ...)
    local p = table_pack(...)
    for i = 1, p.n do
      if type(p[i]) == "number" then
        p.n = i - 1
        break
      end
    end
    if p.n == 0 then
      p[1] = match
      p.n = 1
    end
    n = n + 1
    local v = replacer(match, p)
    return v and v or match
  end))
  return result, n
end

local string = {
  len = string_len;
  byte = string_byte;
  char = string_char;
  sub = string_sub;
  gsub = string_gsub;
}

string_metatable = {
  __index = string;
}

_ENV.string = string
