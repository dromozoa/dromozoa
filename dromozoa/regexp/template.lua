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
  local fcall
  local freturn
  local push
  local clear
  local append
  local append_range
  local guard_clear
  local guard_append
  local guard_append_range

                -- save/restore
                --  | read only
                --  |  |
  local ts      --  x  x  token symbol
  local fs = 1  --  x  x  start position
  local fp      --     x  current position
  local fc      --     x  current character
  local ln = 1  --        line number
  local lp = 0  --        line position

  local action_data = (function ()
    $custom_data
    return { $action_data }
  end)()

  local _, source, source_name, eof_symbol, fn = coroutine.yield()
  local table_unpack = table.unpack or unpack

  local main = _.main
  local action_threads = _.action_threads

  local stack = {}
  local start_line = 1
  local start_column = 1
  local current_position = 1
  local current_index = main
  local current_state = _[current_index].start_state
  local current_cont
  local current_thread
  local jumped = false
  local pushed
  local buffer = {}
  local guard_buffer = {}

  function fcall(index)
    stack[#stack + 1] = {
      token_symbol = ts;
      start_position = fs;
      start_line = start_line;
      start_column = start_column;
      current_index = current_index;
      current_state = current_state;
      current_cont = current_cont;
      current_thread = current_thread;
    }

    if #stack > 2000 then
      error(source_name .. ":" .. start_line .. ":" .. start_column .. ": regexp error (too much recursion; possible loop detected)")
    end

    jumped = true

    ts = nil
    fs = current_position
    start_line = ln
    start_column = fs - lp
    current_index = index
    current_state = _[current_index].start_state
    current_cont = nil

    if current_thread ~= nil then
      current_thread = nil
      coroutine.yield()
    end
  end

  function freturn()
    local item = stack[#stack]
    stack[#stack] = nil

    jumped = true

    ts = item.token_symbol
    fs = item.start_position
    start_line = item.start_line
    start_column = item.start_column
    current_index = item.current_index
    current_state = item.current_state
    current_cont = item.current_cont

    current_thread = item.current_thread
    if current_thread ~= nil then
      assert(coroutine.resume(current_thread))
    end

    if current_cont ~= nil then
      current_cont()
    end
  end

  function push(value_from_buffer)
    local s = string.sub(source, fs, fp)
    local v = s
    if value_from_buffer then
      v = string.char(table_unpack(buffer))
    end
    pushed = fn {
      [0] = ts;
      i = fs;
      j = fp;
      f = source_name;
      n = start_line;
      c = start_column;
      s = s;
      v = v;
    }
  end

  function clear(...)
    buffer = {...}
  end

  function append(...)
    for i = 1, select("#", ...) do
      buffer[#buffer + 1] = select(i, ...)
    end
  end

  function append_range(i, j)
    append(string.byte(source, i, j))
  end

  function guard_clear(...)
    guard_buffer = {...}
  end

  function guard_append(...)
    for i = 1, select("#", ...) do
      guard_buffer[#guard_buffer + 1] = select(i, ...)
    end
  end

  function guard_append_range(i, j)
    guard_append(string.byte(source, i, j))
  end

  local function execute(index, cont)
    local action = action_data[index]
    current_cont = cont
    jumped = false
    if action_threads[index] == 0 then
      current_thread = nil
      action()
    else
      current_thread = coroutine.create(action)
      assert(coroutine.resume(current_thread))
    end
    return jumped
  end

  local function restart()
    if current_state == _[current_index].start_state then
      error(source_name .. ":" .. start_line .. ":" .. start_column .. ": regexp error (loop detected)")
    end

    ts = nil
    fs = current_position
    start_line = ln
    start_column = fs - lp
    current_state = _[current_index].start_state
  end

  local function accept(current_byte)
    if current_state > _[current_index].max_accept_state then
      local near = current_byte == nil and "eof" or string.char(current_byte)
      error(source_name .. ":" .. start_line .. ":" .. start_column .. ": regexp error (cannot transition near " .. near .. ")")
    end

    if execute(_[current_index].accept_actions[current_state], restart) then
      return
    end

    if current_byte == nil then
      if current_index == main then
        ts = eof_symbol
        push()
        return true
      end
      error(source_name .. ":" .. start_line .. ":" .. start_column .. ": regexp error (unexpected eof)")
    end

    restart()
  end

  local function transition()
    local current_byte = string.byte(source, current_position)
    if current_byte == nil then
      return accept(current_byte)
    end
    local s = _[current_index].transitions[current_byte][current_state]
    if s == 0 then
      return accept(current_byte)
    end

    fp = current_position
    fc = current_byte
    current_position = current_position + 1

    if s > _[current_index].max_state then
      local t = s - _[current_index].max_state
      current_state = _[current_index].transition_states[t]
      execute(_[current_index].transition_actions[t])
    else
      current_state = s
    end
  end

  local function guard()
    if _[current_index].guard_action == nil then
      return transition()
    end
    if current_state ~= _[current_index].start_state then
      return transition()
    end

    for i = 1, #guard_buffer do
      if string.byte(source, current_position + i - 1) ~= guard_buffer[i] then
        return transition()
      end
    end

    fp = current_position + #guard_buffer - 1
    fc = guard_buffer[#guard_buffer]
    current_position = current_position + #guard_buffer

    execute(_[current_index].guard_action)
  end

  repeat until guard()
  return pushed
end

local _ = { $shared_data }
local static_data = { $static_data }

return setmetatable({}, {
  __index = static_data;
  __call = function (_, source, source_name, eof_symbol, fn)
    local thread = coroutine.create(main)
    assert(coroutine.resume(thread))
    return select(2, assert(coroutine.resume(thread, static_data, source, source_name, eof_symbol, fn)))
  end;
})
