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

local main = function (_, source, source_name, eof_symbol, fn)
  local fcall
  local freturn
  local ferror
  local fassert
  local push
  local clear
  local append
  local append_range
  local append_unicode
  local guard_clear
  local guard_append
  local guard_append_range

               -- save/restore
               --  | read only
               --  |  |
  local ts     --  x  x  token symbol
  local fs = 1 --  x  x  start position
  local fp     --     x  current position
  local fc     --     x  current character
  local ln = 1 --        line number
  local lp = 0 --        line position

  local action_data = (function ()
    $custom_data
    return { $action_data }
  end)()

  local table_unpack = table.unpack or unpack

  local main = _.main
  local action_continuations = _.action_continuations

  local stack = {}
  local start_line = 1
  local start_column = 1
  local current_position = 1
  local current_index = main
  local current_state = _[current_index].start_state
  local current_cont
  local current_reset
  local current_byte
  local jumped
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
      current_reset = current_reset;
    }

    if #stack > 2000 then
      ferror "too much recursion; possible loop detected"
    end

    ts = nil
    fs = current_position
    start_line = ln
    start_column = fs - lp
    current_index = index
    current_state = _[current_index].start_state
    current_cont = nil
    jumped = true
  end

  function freturn()
    local item = stack[#stack]
    stack[#stack] = nil

    ts = item.token_symbol
    fs = item.start_position
    start_line = item.start_line
    start_column = item.start_column
    current_index = item.current_index
    current_state = item.current_state
    current_cont = item.current_cont
    current_reset = item.current_reset
    jumped = true

    if current_cont > 0 then
      local action = current_cont
      local cont = action_continuations[action]
      current_cont = cont
      action_data[action]()
      if cont > 0 then
        return
      end
    end

    if current_reset then
      ts = nil
      fs = current_position
      start_line = ln
      start_column = fs - lp
      current_state = _[current_index].start_state
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

  function ferror(message)
    if current_byte then
      error(source_name .. ":" .. start_line .. ":" .. start_column .. ": regexp error (" .. message .. " near '" .. string.char(current_byte) .. "')")
    else
      error(source_name .. ":" .. start_line .. ":" .. start_column .. ": regexp error (" .. message .. ")")
    end
  end

  function fassert(v, message, ...)
    if v then
      return v, message, ...
    else
      ferror(message)
    end
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

  if utf8 and utf8.char then
    function append_unicode(a)
      append(string.byte(utf8.char(a), 1, -1))
    end
  else
    function append_unicode(a)
      local n = #buffer + 1
      if a <= 0x7F then
        buffer[n] = a
      elseif a <= 0x7FF then
        local b = a % 0x40
        buffer[n] = (a - b) / 0x40 + 0xC0
        buffer[n + 1] = b + 0x80
      elseif a <= 0xFFFF then
        local c = a % 0x40 a = (a - c) / 0x40
        local b = a % 0x40
        buffer[n] = (a - b) / 0x40 + 0xE0
        buffer[n + 1] = b + 0x80
        buffer[n + 2] = c + 0x80
      elseif a <= 0x1CFFFF then
        local d = a % 0x40 a = (a - d) / 0x40
        local c = a % 0x40 a = (a - c) / 0x40
        local b = a % 0x40
        buffer[n] = (a - b) / 0x40 + 0xF0
        buffer[n + 1] = b + 0x80
        buffer[n + 2] = c + 0x80
        buffer[n + 3] = d + 0x80
      elseif a <= 0x3FFFFFF then
        local e = a % 0x40 a = (a - e) / 0x40
        local d = a % 0x40 a = (a - d) / 0x40
        local c = a % 0x40 a = (a - c) / 0x40
        local b = a % 0x40
        buffer[n] = (a - b) / 0x40 + 0xF8
        buffer[n + 1] = b + 0x80
        buffer[n + 2] = c + 0x80
        buffer[n + 3] = d + 0x80
        buffer[n + 4] = e + 0x80
      else
        local f = a % 0x40 a = (a - f) / 0x40
        local e = a % 0x40 a = (a - e) / 0x40
        local d = a % 0x40 a = (a - d) / 0x40
        local c = a % 0x40 a = (a - c) / 0x40
        local b = a % 0x40
        buffer[n] = (a - b) / 0x40 + 0xFC
        buffer[n + 1] = b + 0x80
        buffer[n + 2] = c + 0x80
        buffer[n + 3] = d + 0x80
        buffer[n + 4] = e + 0x80
        buffer[n + 5] = f + 0x80
      end
    end
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

  local function execute(action, reset)
    current_cont = action_continuations[action]
    current_reset = reset
    jumped = nil
    action_data[action]()
    return jumped
  end

  local function accept()
    if current_state > _[current_index].max_accept_state then
      ferror "cannot transition"
    end

    if execute(_[current_index].accept_actions[current_state], true) then
      return
    end

    if not current_byte then
      if current_index == main then
        ts = eof_symbol
        push()
        return true
      end
      ferror "unexpected eof"
    end

    ts = nil
    fs = current_position
    start_line = ln
    start_column = fs - lp
    current_state = _[current_index].start_state
  end

  local function transition()
    current_byte = string.byte(source, current_position)
    if not current_byte then
      return accept()
    end
    local s = _[current_index].transitions[current_byte][current_state]
    if s == 0 then
      return accept()
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
    if not _[current_index].guard_action then
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
    return main(static_data, source, source_name, eof_symbol, fn)
  end;
})
