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

local class = {}
local metatable = { __index = class }

local function visit(node, accept)
  local code = node[1]
  local a = node[2]
  local b = node[3]

  --[[

    vertex

    edge

  ]]

  if code == "[" then
    -- transition = { set, to }

  elseif code == "." then
    node.u = a.u
    node.v = b.v
    -- epsilon = a.v -> b.u
  elseif code == "|" then
    local u = 0 -- new state
    local v = 0 -- new state
    -- epsilon = u -> a.u
    -- epsilon = u -> b.u
    -- epsilon = a.v -> v
    -- epsilon = b.v -> v
  elseif code == "*" then
    local u = 0 -- new state
    local v = 0 -- new state
    -- epsilon = u -> a.u
    -- epsilon = u -> b.u
    -- epsilon = a.v -> v
    -- epsilon = b.v -> v

  elseif code == "?" then
  else
  end
end

return function (root, accept)
  local graph = {}
  local max_state = visit(graph, 0, root, accept)
  graph.max_state = max_state
  return graph
end
