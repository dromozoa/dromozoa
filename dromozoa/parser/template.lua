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

local main = function ()
  local create
  local append

  local S
  local SS

  $custom_data
  local action_data = { $action_data }
  local table_unpack = table.unpack or unpack
  local static_data = coroutine.yield()

  local symbol_names = static_data.symbol_names
  local actions = static_data.actions
  local max_state = #actions
  local heads = static_data.heads
  local sizes = static_data.sizes
  local semantic_actions = static_data.semantic_actions

  local stack = { 1 }
  local nodes = {}

  function create(symbol)
    return { [0] = symbol }
  end

  function append(...)
    for i = 1, select("#", ...) do
      SS[#SS + 1] = select(i, ...)
    end
  end

  while true do
    local token = coroutine.yield()
    local symbol = token[0]
    if symbol ~= nil then
      while true do
        local state = stack[#stack]
        local action = actions[state][symbol]
        if action == 0 then
          local at = ""
          if token.f ~= nil and token.n ~= nil and token.c ~= nil and token.s ~= nil then
            error(token.f .. ":" .. token.n .. ":" .. token.c .. ": parser error (cannot transition near " .. token.s .. ")")
          else
            error("parser error (cannot transition near " .. symbol_names[symbol] .. ")")
          end
        end

        -- shift
        if action <= max_state then
          stack[#stack + 1] = action
          nodes[#nodes + 1] = token
          break
        end

        local index = action - max_state
        -- accept
        if index == 1 then
          local accepted_node = nodes[#nodes]
          stack[#stack] = nil
          nodes[#nodes] = nil
          return accepted_node
        end

        -- reduce
        local head = heads[index]
        local size = sizes[index]

        local rf
        local ri
        local rj
        local rn
        local rc

        S = { [0] = create(head) }
        SS = create(head)

        for i = 1, size do
          local node = nodes[#nodes - size + i]
          S[i] = node
          SS[i] = node

          if rf == nil then
            rf = node.f
            ri = node.i
            rj = node.j
            rn = node.n
            rc = node.c
          elseif rf == node.f then
            if ri == nil or ri > node.i then
              ri = node.i
            end
            if rj == nil or rj < node.j then
              rj = node.j
            end
            if rn == nil or rn > node.n then
              rn = node.n
              rc = node.c
            elseif rn == node.n and (rc == nil or rc > node.c) then
              rc = node.c
            end
          end
        end

        for i = 1, size do
          stack[#stack] = nil
          nodes[#nodes] = nil
        end

        SS.f = rf
        SS.i = ri
        SS.j = rj
        SS.n = rn
        SS.c = rc

        action_data[semantic_actions[index]]()

        local state = stack[#stack]
        stack[#stack + 1] = actions[state][head]
        nodes[#nodes + 1] = SS
      end
    end
  end
end

local static_data = { $static_data }

local metatable = {
  __call = function (self, token)
    return select(2, assert(coroutine.resume(self.thread, token)))
  end;
}

return setmetatable({}, {
  __index = static_data;
  __call = function ()
    local thread = coroutine.create(main)
    assert(coroutine.resume(thread))
    assert(coroutine.resume(thread, static_data))
    return setmetatable({ thread = thread }, metatable)
  end;
})
