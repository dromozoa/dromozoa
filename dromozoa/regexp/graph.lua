-- Copyright (C) 2021 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local class = {}
local metatable = { __index = class }

local function new()
  local self = {
    uid = 0;
    eid = 0;

    u = {};
    e = {};
    uv = {};
  }
  return self
end

function class:new_vertex()
  local uid = self.uid + 1
  self.uid = uid
  return uid
end

function class:new_edge(u, v)
  local eid = self.eid + 1
  self.eid = eid
  return eid
end

return setmetatable(class, {
  __call = function ()
    return setmetatable(new(), metatable)
  end;
})
