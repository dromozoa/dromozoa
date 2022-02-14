-- Copyright (C) 2020-2022 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local graph = require "dromozoa.regexp.graph"

local function map_to_seq(map)
  local seq = {}
  for id, node in pairs(map) do
    seq[#seq + 1] = { id = id, node = node }
  end
  table.sort(seq, function (a, b) return a.id < b.id end)
  local key = {}
  for i = 1, #seq do
    key[i] = seq[i].id
  end
  seq.key = table.concat(key, ",")
  return seq
end

local function visit(u, map, state_to_index)
  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    if not transition.set then
      local v = transition.v
      local vid = state_to_index[v]
      map[vid] = v
      visit(v, map, state_to_index)
    end
  end
end

local function epsilon_closure(u, epsilon_closures, state_to_index)
  local seq = epsilon_closures[u]
  if not seq then
    local map = { [state_to_index[u]] = u }
    visit(u, map, state_to_index)
    seq = map_to_seq(map)
    epsilon_closures[u] = seq
  end
  return seq
end

local function visit(useq, map, epsilon_closures, state_to_index, color)

  -- set
  -- seq
  -- str

  -- local rev_transition = {}
  -- local map_transition = {}

  local map_transitions = {}
  local new_transitions = {}

  color[useq] = 1

  for byte = 0x00, 0xFF do
    local vmap = {}

    for i = 1, #useq do
      local item = useq[i]
      local xid = item.id
      local x = item.node
      local transitions = x.transitions
      for j = 1, #transitions do
        local transition = transitions[j]
        local set = transition.set
        if set and set[byte] then
          local y = transition.v
          local yid = state_to_index[y]
          local zseq = epsilon_closure(y, epsilon_closures, state_to_index)
          for k = 1, #zseq do
            local item = zseq[k]
            local zid = item.id
            local z = item.node
            vmap[zid] = z
          end
        end
      end
    end

    if next(vmap) then
      local vseq = map_to_seq(vmap)
      local vkey = vseq.key
      local vobj = map[vkey]
      if not vobj then
        vobj = { graph.new_state(), vseq }
        map[vkey] = vobj
      end

      local new_transition = map_transitions[vobj]
      if not new_transition then
        new_transition = { v = vobj, set = {} }
        map_transitions[vobj] = new_transition
        new_transitions[#new_transitions + 1] = new_transition
      end
      new_transition.set[byte] = true
    end
  end

  local uobj = assert(map[useq.key])
  for i = 1, #new_transitions do
    local new_transition = new_transitions[i]
    local vobj = new_transition.v
    local vset = new_transition.set
    graph.new_transition(uobj[1], vobj[1], vset)

    local vseq = vobj[2]

    -- merge accept state
    -- vsetに含まれる最大のacceptをvobjに設定する
    local accept
    for i = 1, #vseq do
      local yid = vseq[i].id
      local y = vseq[i].node
      local a = y.accept
      if a and (not accept or accept > a) then
        accept = a
      end
    end
    vobj[1].accept = accept

    if not color[vseq] then
      visit(vseq, map, epsilon_closures, state_to_index, color)
    end
  end
end

return function (u)
  local state_to_index = graph.create_state_indices(u)

  local epsilon_closures = {}
  local useq = epsilon_closure(u, epsilon_closures, state_to_index)
  local uobj = graph.new_state()

  local map = { [useq.key] = { uobj, useq } }
  visit(useq, map, epsilon_closures, state_to_index, {})

  return uobj
end
