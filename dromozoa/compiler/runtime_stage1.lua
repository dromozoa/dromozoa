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
  elseif D.is_table(v) then
    return "table"
  elseif D.is_function(v) then
    return "function"
  else
    return "userdata"
  end
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
  local n = D.select_n(...)
  if n == 0 then
    G.console:log()
  else
    G.console:log(concat(n, ...))
  end
end





