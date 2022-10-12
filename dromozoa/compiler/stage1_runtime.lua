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
local F = G.fs

---------------------------------------------------------------------------

local function assert(...)
  if ... then
    return ...
  else
    local _, message = ...
    if message == nil then
      return error "assertion failed!"
    else
      return error(message)
    end
  end
end

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
    local metafield = D.getmetafield(v, "__tostring")
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
  local metafield = D.getmetafield(v, "__name")
  if type(metafield) == "string" then
    t = metafield
  end
  return t .. ": " .. v
end

local function print(...)
  local list = { ... }
  for i = 1, select("#", ...) do
    if i > 1 then
      F:writeSync(1, "\t")
    end
    F:writeSync(1, tostring(list[i]))
  end
  F:writeSync(1, "\n")
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
  local metafield = D.getmetafield(t, "__pairs")
  if metafield == nil then
    return pairs_impl, D.native_call(t, "entries")
  else
    local f, s, var = metafield(t)
    return f, s, var
  end
end

_ENV.assert = assert
_ENV.tostring = tostring
_ENV.print = print
_ENV.require = require
_ENV.ipairs = ipairs
_ENV.pairs = pairs

---------------------------------------------------------------------------

local function table_pack(...)
  return { n = select("#", ...), ... }
end

local function table_unpack(list, i, j)
  if i == nil then
    i = 1
  end
  if j == nil then
    j = #list
  end
  return D.array_unpack(D.array_from(list, i, j))
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
  return D.array_from(list, i, j):join(sep)
end

local function table_sort(list, comp)
  if comp == nil then
    comp = function (a, b) return a < b end
  end
  local n = #list
  local array = D.array_from(list, 1, n)
  array:sort(D.native_function(function (a, b)
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

_ENV.table = table

---------------------------------------------------------------------------

local string_encode_cache = {}

local function string_encode_utf8(s)
  local b = string_encode_cache[s]
  if b ~= nil then
    return b
  end
  local b = G.Buffer:from(s)
  string_encode_cache[s] = b
  return b
end

local function string_len(s)
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
  local b = string_encode_utf8(s)
  if i == nil then
    i = 1
  end
  if j == nil then
    j = i
  end
  local i, j = string_prepare(b.length, i, j)
  if i == j then
    return b[i - 1]
  else
    return D.array_unpack(G.Array:from(b:subarray(i - 1, j)))
  end
end

local function string_char(...)
  return G.Buffer:from(D.array_pack(...)):toString()
end

local function string_sub(s, i, j)
  local b = string_encode_utf8(s)
  if j == nil then
    j = -1
  end
  local i, j = string_prepare(b.length, i, j)
  return b:toString("utf8", i - 1, j)
end

local function string_gsub_regexp(pattern)
  return D.native_new(G.RegExp, pattern, "gs")
end

local RE1 = string_gsub_regexp [[\\]]
local RE2 = string_gsub_regexp [[\%z]]
local RE3 = string_gsub_regexp [[\%(.)]]
local RE4 = string_gsub_regexp [[\.\-]]

local function string_gsub_pattern(s)
  s = D.native_call(s, "replace", RE1, [[\\]])
  s = D.native_call(s, "replace", RE2, [[\u0000]])
  s = D.native_call(s, "replace", RE3, [[\$1]])
  s = D.native_call(s, "replace", RE4, [[.*?]])
  return s
end

local RE = string_gsub_regexp [[\%([\%0-9])]]

local function string_gsub_string(repl, match, p)
  return D.native_call(repl, "replace", RE, D.native_function(function (a, b)
    if b == "%" then
      return "%"
    elseif b == "0" then
      return match
    else
      local v = p[G:Number(b)]
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
  local result = D.native_call(s, "replace", re, D.native_function(function (match, ...)
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

D.string_metatable.__index = string
_ENV.string = string

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class }

function class:write(s)
  local fd = assert(self.fd)
  F:writeSync(fd, s)
end

function class:read(p)
  assert(p == "*a")
  local fd = assert(self.fd)
  local s = F:fstatSync(fd)
  local b = D.native_new(G.Uint8Array, s.size)
  F:readSync(fd, b)
  return G.Buffer:from(b):toString()
end

function class:close()
  local fd = self.fd
  self.fd = nil
  F:closeSync(fd)
end

local function io_open(filename, mode)
  if mode == nil then
    mode = "r"
  end
  local fd = F:openSync(filename, mode)
  return setmetatable({ fd = fd }, metatable)
end

local io = {
  open = io_open;
}

_ENV.io = io
