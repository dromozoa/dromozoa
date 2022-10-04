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
  elseif message ~= nil then
    return error(message)
  else
    return error "assertion failed!"
  end
end

function select(index, v, ...)
  if index == "#" then
    return D.select(v, ...)
  elseif index == 1 then
    return v, ...
  else
    return select(index - 1, ...)
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

local function getmetafield(object, event)
  local t = type(object)
  if t == "table" then
    local metatable = D.getmetatable(object)
    if metatable ~= nil then
      return metatable[event]
    end
  end
end

function getmetatable(object)
  local t = type(object)
  if t == "table" then
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
  assert(getmetafield(table, "__metatable") == nil, "cannot change a protected metatable")
  D.setmetatable(table, metatable)
  return table
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
    local metamethod = getmetafield(v, "__tostring")
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
  local metafield = getmetafield(v, "__name")
  if type(metafield) == "string" then
    t = metafield
  end
  return t .. ": " .. v
end

local function concat(i, v, ...)
  if v == nil then
    v = "nil"
  elseif v == false then
    v = "false"
  elseif v == true then
    v = "true"
  end

  if i == 1 then
    return v
  else
    return v .. "\t" .. concat(i - 1, ...)
  end
end

function print(...)
  -- local result = table.pack(...)
  local result = { ... }
  result.n = select("#", ...)
  for i = 1, result.n do
    result[i] = tostring(result[i])
  end
  G.console:log(table.concat(result, "\t", 1, result.n))
end

function require(name)
  if package.loaded == nil then
    package.loaded = {}
  end

  local module = package.loaded[name]
  if module == nil then
    module = package.preload[name]()
    package.loaded[name] = module
  end

  return module
end

---------------------------------------------------------------------------

local function table_unpack(list, i, j)
  if i < j then
    return list[i], table_unpack(list, i + 1, j)
  else
    return list[i]
  end
end

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
}


