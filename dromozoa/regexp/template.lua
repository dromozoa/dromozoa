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

local _ = { $shared_data }
local static_data = { $static_data }
local _

return function (source, source_name, fn)
  local fcall
  local fret
  local push_token
  local skip_token

  local fs = 1  -- start position
  local fp      -- current position
  local fc      -- current character
  local fb = {} -- string buffer
  local fg = {} -- guard buffer
  local ln = 1  -- line number
  local lp = 0  -- line position
  local ra
  local rb
  local rc
  local rd

  local _ = (function ()
    local static_data
    local source
    local source_name
    local fn

    return { $action_data }
  end)()

  for i, u in ipairs(static_data) do
    for k, v in pairs(u) do
      _[i][k] = v
    end
  end

  local main = static_data.main

  local start_line = 1
  local start_column = 1
  local current_position = 1
  local current_byte
  local current_index = static_data.main
  local current_state = _[current_index].start_state
  local current_loop

  local top = 0
  local stack = {}
  local tokens = {}

  function fcall(index)
    coroutine.yield(index)
  end

  function fret()
    local item = stack[#stack]
    stack[#stack] = nil
    coroutine.resume(item.thread)
  end

  while true do
    local guard_action = _[current_index].guard_action
    if guard_action ~= nil then
      if current_state == _[current_index].start_state then
      end
    end

    current_byte = string.byte(source, current_position)

    local s = 0
    if current_byte ~= nil then
      s = _[current_index].transitions[current_byte][current_state]
    end

    if s == 0 then
    else
      fp = current_position
      fc = current_byte
      if s > _[current_index].max_state then
        local t = s - _[current_index].max_state
        current_position = current_position + 1
        current_state = _[current_index].transition_states[t]
        -- coroutine
        thread = coroutine.create(_[current_index].transition_actions[t])
        local _, index = assert(coroutine.resume(thread))
        stack[#stack + 1] = {
          index = index;
          thread = thread;
        }
      else
        current_position = current_position + 1
        current_state = s
      end
    end
  end

end
