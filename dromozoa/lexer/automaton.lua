-- Copyright (C) 2020 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local encode_char_class = require "dromozoa.lexer.encode_char_class"

local class = {}
local metatable = { __index = class }

function class.nfa()
  local transitions = {}
  for byte = 0x00, 0xFF do
    transitions[byte] = {}
  end
  return setmetatable({
    max_state = 0;
    epsilons1 = {};
    epsilons2 = {};
    transitions = transitions;
    start_state = nil;
    accept_states = {};
  }, metatable)
end

function class:new_state()
  local max_state = self.max_state + 1
  self.max_state = max_state
  return max_state
end

function class:new_transition(u, v, set)
  if set then
    local transitions = self.transitions
    for byte in pairs(set) do
      transitions[byte][u] = v
    end
  else
    local epsilons = self.epsilons1
    if epsilons[u] then
      epsilons = self.epsilons2
    end
    epsilons[u] = v
  end
end

function class:write_graphviz(out)
  local epsilons1 = self.epsilons1
  local epsilons2 = self.epsilons2
  local transitions = self.transitions
  local start_state = self.start_state
  local accept_states = self.accept_states

  out:write [[
digraph {
graph[rankdir=LR];
]]

  local u = start_state
  out:write(u, "[style=filled,fillcolor=black,fontcolor=white")
  local accept = accept_states[u]
  if accept then
    out:write(",peripheries=2,label=\"", u, "/", accept, "\"")
  end
  out:write "];\n"

  for u = 1, self.max_state do
    if u ~= start_state then
      local accept = accept_states[u]
      if accept then
        out:write(u, "[peripheries=2,label=\"", u, "/", accept, "\"];\n")
      end
    end

    local v = epsilons1[u]
    if v then
      out:write(u, "->", v, ";\n")
    end
    local v = epsilons2[u]
    if v then
      out:write(u, "->", v, ";\n")
    end
    local set_table = {}
    for byte = 0x00, 0xFF do
      local v = transitions[byte][u]
      if v then
        local set = set_table[v]
        if set then
          set[byte] = true
        else
          set_table[v] = { [byte] = true }
        end
      end
    end
    for v, set in pairs(set_table) do
      out:write(u, "->", v, "[label=\"", encode_char_class(set), "\"];\n")
    end
  end

  out:write "}\n"

  return out
end

return class
