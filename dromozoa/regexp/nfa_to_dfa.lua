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

local graph = require "dromozoa.regexp.graph"

local function visit(u, epsilon_closure, state_to_index)
  local transitions = u.transitions
  for i = 1, #transitions do
    local transition = transitions[i]
    if not transition.set then
      local v = transition.v
      local vid = state_to_index[v]
      epsilon_closure[vid] = true
      visit(v, epsilon_closure, state_to_index)
    end
  end
end

local function epsilon_closure(u, epsilon_closures, state_to_index)
  local epsilon_closure = epsilon_closures[u]
  if not epsilon_closure then
    local uid = state_to_index[u]
    epsilon_closure = { [uid] = true }
    epsilon_closures[u] = epsilon_closure
    visit(u, epsilon_closure, state_to_index)
  end
  return epsilon_closure
end

local function set_to_seq(set)
  local seq = {}
  for k in pairs(set) do
    seq[#seq + 1] = k
  end
  table.sort(seq)
  return seq
end

local function encode_seq(seq)
  return table.concat(seq, ",")
end

local function visit(useq, map, epsilon_closures, color, rev_color)
  local new_transition = {}
  local rev_transition = {}
  local map_transition = {}

  for byte = 0x00, 0xFF do
    local vset = {}
    for i = 1, #useq do
      local xid = useq[i]
      local x = rev_color[xid]
      local transitions = x.transitions
      for j = 1, #transitions do
        local transition = transitions[j]
        local set = transition.set
        if set then
          if set[byte] then
            local y = transition.v
            local yid = color[y]

            local zset = epsilon_closure(y, epsilon_closures, color)
            for zid in pairs(zset) do
              vset[zid] = true
            end
          end
        end
      end
    end
    if next(vset) then
      local vseq = set_to_seq(vset)
      local vstr = encode_seq(vseq)
      local vobj = map[vstr]
      if vobj then
        new_transition[byte] = vobj
        if not rev_transition[vobj] then
          rev_transition[vobj] = {}
        end
        rev_transition[vobj][byte] = true
      else
        vobj = graph.new_state()
        map[vstr] = vobj

        new_transition[byte] = vobj
        if not rev_transition[vobj] then
          rev_transition[vobj] = {}
        end
        rev_transition[vobj][byte] = true
        map_transition[#map_transition + 1] = vobj

        vobj.seq = vseq
      end
    end
  end

  local uobj = assert(map[encode_seq(useq)])
  for i = 1, #map_transition do
    local vobj = map_transition[i]
    local tset = rev_transition[vobj]
    graph.new_transition(uobj, vobj, tset)
    -- print(uobj, vobj, encode_seq(set_to_seq(tset)))

    local vseq = vobj.seq

    -- merge accept state
    -- vsetに含まれる最大のacceptをvobjに設定する
    local accept
    for i = 1, #vseq do
      local yid = vseq[i]
      local y = rev_color[yid]
      local a = y.accept
      if a and (not accept or accept > a) then
        accept = a
      end
    end
    vobj.accept = accept

    visit(vseq, map, epsilon_closures, color, rev_color)
  end
end

return function (u)
  local state_to_index, index_to_state = graph.create_state_indices(u)

  local epsilon_closures = {}
  local uset = epsilon_closure(u, epsilon_closures, state_to_index)
  local useq = set_to_seq(uset)
  local uobj = graph.new_state()

  local map = { [encode_seq(useq)] = uobj }
  visit(useq, map, epsilon_closures, state_to_index, index_to_state)

  return uobj
end
