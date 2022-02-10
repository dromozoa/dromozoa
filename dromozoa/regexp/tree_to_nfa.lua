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

local edge = require "dromozoa.regexp.edge"
local vertex = require "dromozoa.regexp.vertex"

local class = {}
local metatable = { __index = class }

local function visit(node)
  local code = node[1]
  local a = node[2]
  local b = node[3]

  if code == "[" then
    local u = vertex.new()
    local v = vertex.new()
    local t = edge.new(u, v)
    t.set = a
    node.u = u
    node.v = v
  else
    visit(a)
    if b then
      visit(b)
    end
    if code == "." then
      edge.new(a.v, b.u)
      node.u = a.u
      node.v = b.v
    elseif code == "|" then
      local u = vertex.new()
      local v = vertex.new()
      edge.new(u, a.u)
      edge.new(u, b.u)
      edge.new(a.v, v)
      edge.new(b.v, v)
      node.u = u
      node.v = v
    elseif code == "*" then
      local u = a.u
      local v = a.v
      edge.new(u, v)
      edge.new(v, u)
      node.u = u
      node.v = v
    elseif code == "?" then
      local u = a.u
      local v = a.v
      edge.new(u, v)
      node.u = u
      node.v = v
    end
  end

  return node.u
end

return visit
