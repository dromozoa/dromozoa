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

local main = function (static_data)
  local create
  local append
  local append_unpack

  local S
  local SS

  local action_data = (function ()
    $custom_data
    return { $action_data }
  end)()

  local error = error
  local select = select
  local table_unpack = table.unpack or unpack

  local symbol_names = static_data.symbol_names
  local actions = static_data.actions
  local max_state = #actions
  local heads = static_data.heads
  local sizes = static_data.sizes
  local semantic_actions = static_data.semantic_actions

  local stack = { 1 }
  local nodes = { false }
  local n = 1

  function create(symbol)
    return { [0] = symbol }
  end

  function append(...)
    for i = 1, select("#", ...) do
      SS[#SS + 1] = select(i, ...)
    end
  end

  function append_unpack(...)
    for i = 1, select("#", ...) do
      append(table_unpack((select(i, ...))))
    end
  end

  return function (token)
    local symbol = token[0]
    if symbol then
      while true do
        local state = stack[n]
        local action = actions[state][symbol]
        if action == 0 then
          if token.f and token.n and token.c and token.s then
            error(token.f..":"..token.n..":"..token.c..": parser error (cannot transition near '"..token.s.."')")
          else
            error("parser error (cannot transition near '"..symbol_names[symbol].."')")
          end
        end

        -- shift
        if action <= max_state then
          n = n + 1
          stack[n] = action
          nodes[n] = token
          break
        end

        local index = action - max_state

        -- accept
        if index == 1 then
          local node = nodes[n]
          n = n - 1
          return node
        end

        -- reduce
        local head = heads[index]
        local size = sizes[index]

        S = { [0] = { [0] = head } }
        SS = { [0] = head }
        local sf, si, sj, sn, sc

        n = n - size
        for i = 1, size do
          local s = nodes[n + i]
          S[i] = s
          SS[i] = s

          if not sf then
            sf, si, sj, sn, sc = s.f, s.i, s.j, s.n, s.c
          elseif sf == s.f then
            if not si or si > s.i then
              si = s.i
            end
            if not sj or sj < s.j then
              sj = s.j
            end
            if not sn or sn > s.n or (sn == s.n and (not sc or sc > s.c)) then
              sn, sc = s.n, s.c
            end
          end
        end

        action_data[semantic_actions[index]]()
        SS.f, SS.i, SS.j, SS.n, SS.c = sf, si, sj, sn, sc

        local state = stack[n]
        n = n + 1
        stack[n] = actions[state][head]
        nodes[n] = SS
      end
    end
  end
end

local static_data = { $static_data }

return setmetatable({}, {
  __index = static_data;
  __call = function (static_data)
    return main(static_data)
  end;
})
