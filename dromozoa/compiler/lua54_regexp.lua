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
  local ts
  local fs = 1
  local fp
  local fc
  local ln = 1
  local lp = 0
  local action_data = (function ()
    local ra
local rb

    return { function()
end;
function()ln=ln+1 lp=fp
end;
function()lp=fp
end;
function()freturn()
end;
function()append(fc)
end;
function()append(0x0A) ln=ln+1 lp=fp
end;
function()fassert(ra<=255,'decimal escape too large') append(ra)
end;
function()ra=fc-0x30
end;
function()ra=ra*10+fc-0x30
end;
function()append(0x07)
end;
function()append(0x0C)
end;
function()append(0x0A)
end;
function()append(0x0D)
end;
function()append(0x09)
end;
function()ra=fp rb=0
end;
function()ra=fp
end;
function()rb=rb*16+fc-0x30
end;
function()rb=rb*16+fc-0x41+10
end;
function()rb=rb*16+fc-0x61+10
end;
function()fassert(fp-ra<=9 and rb<=0x7FFFFFFF,'UTF-8 value too large') append_unicode(rb)
end;
function()append(0x0B)
end;
function()append(ra*16+fc-0x30)
end;
function()append(ra*16+fc-0x41+10)
end;
function()append(ra*16+fc-0x61+10)
end;
function()ra=fc-0x41+10
end;
function()ra=fc-0x61+10
end;
function()ts=nil push()
end;
function()ts=2;clear() fcall(3)
end;
function()push(true)
end;
function()ts=35 push()
end;
function()ts=33 push()
end;
function()ts=36 push()
end;
function()ts=49 push()
end;
function()ts=50 push()
end;
function()ts=31 push()
end;
function()ts=29 push()
end;
function()ts=58 push()
end;
function()ts=30 push()
end;
function()ts=nil;fcall(1)
end;
function()push()
end;
function()ts=59 push()
end;
function()ts=60 push()
end;
function()ts=61 push()
end;
function()ts=4 push()
end;
function()ts=32 push()
end;
function()ts=41 push()
end;
function()ts=3 push()
end;
function()ts=6 push()
end;
function()ts=5 push()
end;
function()ts=57 push()
end;
function()ts=55 push()
end;
function()ts=56 push()
end;
function()ts=46 push()
end;
function()ts=39 push()
end;
function()ts=44 push()
end;
function()ts=48 push()
end;
function()ts=42 push()
end;
function()ts=47 push()
end;
function()ts=45 push()
end;
function()ts=40 push()
end;
function()ts=62 push()
end;
function()ts=53 push()
end;
function()ts=1;clear() fcall(2)
end;
function()ts=54 push()
end;
function()ts=34 push()
end;
function()ts=7 push()
end;
function()ts=8 push()
end;
function()ts=9 push()
end;
function()ts=10 push()
end;
function()ts=11 push()
end;
function()ts=12 push()
end;
function()ts=13 push()
end;
function()ts=14 push()
end;
function()ts=15 push()
end;
function()ts=16 push()
end;
function()ts=17 push()
end;
function()ts=18 push()
end;
function()ts=19 push()
end;
function()ts=20 push()
end;
function()ts=21 push()
end;
function()ts=22 push()
end;
function()ts=23 push()
end;
function()ts=24 push()
end;
function()ts=25 push()
end;
function()ts=26 push()
end;
function()ts=27 push()
end;
function()ts=28 push()
end;
function()ts=51 push()
end;
function()ts=38 push()
end;
function()ts=52 push()
end;
function()ts=37 push()
end;
function()ts=43 push()
end;
function()guard_clear(fc)
end;
function()guard_clear(0x5D)
end;
function()guard_append(fc)
end;
function()guard_append(0x5D)
end;
 }
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
  local current_continuation
  local current_restart
  local current_byte
  local jumped = false
  local pushed
  local buffer = {}
  local guard_buffer = {}
  local execute
  function fcall(index)
    stack[#stack + 1] = {
      token_symbol = ts;
      start_position = fs;
      start_line = start_line;
      start_column = start_column;
      current_index = current_index;
      current_state = current_state;
      current_continuation = current_continuation;
      current_restart = current_restart;
    }
    if #stack > 2000 then
      ferror "too much recursion; possible loop detected"
    end
    jumped = true
    ts = nil
    fs = current_position
    start_line = ln
    start_column = fs - lp
    current_index = index
    current_state = _[current_index].start_state
    current_continuation = nil
    current_restart = nil
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
    current_continuation = item.current_continuation
    current_restart = item.current_restart
    if current_continuation then
      local action = action_data[current_continuation]
      if action then
        action()
      end
    end
    if current_restart then
      current_restart()
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
  function execute(index, restart)
    local action = action_data[index]
    current_continuation = action_continuations[index]
    current_restart = restart
    if current_continuation == 0 then
      current_continuation = nil
    end
    jumped = false
    action()
    return jumped
  end
  local function restart()
    if current_state == _[current_index].start_state then
      ferror "loop detected"
    end
    ts = nil
    fs = current_position
    start_line = ln
    start_column = fs - lp
    current_state = _[current_index].start_state
  end
  local function accept()
    if current_state > _[current_index].max_accept_state then
      ferror "cannot transition"
    end
    if execute(_[current_index].accept_actions[current_state], restart) then
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
    restart()
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
local _ = { {0,0,0,1};
{0,0,8,5};
{0,6,0,7};
{2,3,2,3};
{2,1,3,1};
{1,1,1};
{0,0,0,5};
{0,0,9,6};
{0,7,0,8};
{5,6,3,6,3};
{1,2,1,3,1};
{0,0,0,0,0,0,0,0,0,17,0,0,0,0,0,0};
{0,0,0,0,5,5,0,0,0,17,0,0,0,0,0,0};
{0,18,0,21,52,53,0,0,0,17,0,0,0,0,0,0};
{0,20,19,0,55,54,0,0,0,17,0,0,0,0,0,0};
{0,22,0,0,0,0,0,0,0,17,0,0,0,0,0,0};
{0,23,0,0,0,0,24,25,0,17,0,32,33,35,46,47};
{0,23,0,0,0,0,24,25,0,17,0,42,34,35,46,47};
{0,0,0,0,0,0,0,0,0,17,0,43,39,36,50,48};
{0,22,0,0,0,0,0,0,0,2,0,0,0,0,0,0};
{0,26,0,0,0,0,0,0,0,17,0,44,40,37,51,49};
{0,0,0,0,0,0,0,0,0,17,0,44,40,37,51,49};
{0,27,0,0,0,0,0,0,0,17,0,44,40,37,51,49};
{0,28,0,0,0,0,0,0,0,17,0,0,0,0,0,0};
{0,29,0,0,0,0,0,0,0,17,0,0,0,0,0,0};
{0,30,0,0,0,0,0,0,0,17,0,0,0,0,0,0};
{0,11,0,0,0,0,0,0,0,17,0,0,0,0,0,0};
{0,45,0,0,0,0,0,0,0,17,0,0,0,0,0,0};
{0,15,0,0,0,0,0,0,0,17,0,0,0,0,0,0};
{0,5,0,0,0,0,0,0,0,17,0,0,0,0,0,0};
{0,0,0,0,0,0,0,0,0,17,31,0,0,0,0,0};
{0,0,0,0,0,0,0,0,0,17,0,0,41,38,0,0};
{5,6,3,6,3,5,8,9,9,10,11,12,13,14,15,16,16,17,17,18,19,20,18,19,20,17,18,19,21,8,22,23,24,25,26,2,2,3,2};
{1,3,1,4,1,1,7,8,9,1,1,1,1,1,12,13,13,14,14,14,14,1,14,14,1,14,14,14,1,16,1,1,1,16,16,6,6,5,5};
{1,1,1,1,1,1,7,7,7};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{1,1,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0};
{138,139,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,152,0,0,155,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,142,0,0,0,0,0};
{141,140,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,154,153,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,143,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,144,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,28,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13,134,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,3,4,0,0,0,0,0,0,0,0,0,0,28,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,134,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,18,19,0,0,0,0,0,20,20,0,0,0,26,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17,0,0,136,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,23,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,22,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,20,0,0,20,21,0,0,25,25,26,28,28,29,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,24,21,21,29,26,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,20,0,0,20,21,0,0,25,25,26,28,28,29,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,25,21,21,29,26,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,31,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,34,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33,0,0,0,0,0};
{0,0,4,4,146,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,35,0,0,37,0,39,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,149,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,131,0,36,0,0,0,0,150};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,38,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,26,0,0,29,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,41,0,0,29,26,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,133,0,0,0,133,133,26,0,0,29,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,41,0,0,29,26,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,41,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,27,0,0,27,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,41,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,135,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,41,0,0,0,0,0};
{0,0,145,4,147,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,156,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,148,0,0,0,0,151};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,103,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,104,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,26,0,0,29,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,78,41,41,52,41,41,41,88,41,92,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,61,0,0,29,26,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,26,0,0,29,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,45,0,0,29,26,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,26,0,0,29,0,0,0,0,0,0,0,0,0,0,0,41,41,46,41,41,41,41,41,41,41,41,41,41,41,69,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,41,0,0,29,26,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,26,0,0,29,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,105,41,41,41,41,110,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,79,0,0,29,26,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,133,0,0,0,133,133,26,0,0,29,0,0,0,0,0,0,0,0,0,0,0,41,41,41,48,41,41,41,41,41,41,62,41,41,41,41,71,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,94,41,41,41,41,41,41,108,41,41,111,41,41,41,41,41,41,41,41,41,41,41,124,41,126,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,63,0,0,29,26,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,26,0,0,29,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,109,41,41,41,41,41,115,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,65,0,0,29,26,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,54,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,60,41,41,41,41,41,41,41,73,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,41,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,67,41,41,41,41,41,41,76,41,41,41,41,41,41,41,41,41,89,41,41,41,41,96,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,81,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,87,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,106,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,41,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,66,41,41,41,41,41,41,41,41,41,41,64,41,41,41,41,41,41,41,41,41,41,41,41,97,41,41,41,41,41,41,41,41,41,41,41,117,118,41,41,41,41,41,41,125,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,47,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,43,41,41,41,41,41,41,59,41,41,41,41,41,41,41,41,41,41,41,77,41,82,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,113,41,116,41,41,41,41,41,122,123,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,70,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,55,41,41,41,41,41,41,68,41,41,41,41,41,41,41,41,41,41,84,41,85,41,41,90,41,41,41,41,41,41,41,41,107,41,41,41,41,41,41,114,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,91,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,27,0,0,27,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,56,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,41,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,51,41,41,41,41,41,41,41,41,41,41,41,41,74,41,41,41,41,41,41,41,41,41,41,41,41,41,93,41,41,41,41,41,41,41,41,41,41,41,112,41,41,41,41,41,41,120,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,44,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,80,41,83,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,41,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,53,41,57,41,41,41,41,41,41,41,41,41,41,75,41,41,41,41,41,41,41,41,86,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,119,41,121,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,58,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,72,41,41,41,41,41,41,41,42,41,41,41,41,41,41,41,41,95,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,49,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,0,0,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,0,0,0,0,0,50,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,127,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,129,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,130,0,0,0,0,0};
{2,2,3,2,2,2,93,94,95,96,94,95,95,96,2,3,2,3,96};
{2,2,1,1,2,1,6,5,5,16,98,137,137,99,100,101,102,101,99};
{27,27,27,27,27,28,30,31,32,33,34,35,36,37,38,39,41,42,43,44,44,45,46,47,47,48,48,48,49,50,51,52,53,54,55,56,57,58,59,60,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,62,63,63,63,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92};
{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,29,0,0,0,0,0,0,0,0,0,0,40,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,29,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
 }
local static_data = { main=4;
{
start_state=4;
max_accept_state=3;
max_state=4;
transitions={[0]=_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[2],_[1],_[1],_[3],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],_[1],};
transition_actions=_[4];
transition_states=_[5];
accept_actions=_[6];
guard_action=4;
};
{
start_state=4;
max_accept_state=3;
max_state=4;
transitions={[0]=_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[8],_[7],_[7],_[9],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],_[7],};
transition_actions=_[10];
transition_states=_[11];
accept_actions=_[6];
guard_action=4;
};
{
start_state=10;
max_accept_state=9;
max_state=16;
transitions={[0]=_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[13],_[14],_[13],_[13],_[15],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[13],_[12],_[16],_[12],_[12],_[12],_[12],_[16],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[17],_[18],_[18],_[18],_[18],_[18],_[18],_[18],_[18],_[18],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[19],_[19],_[19],_[19],_[19],_[19],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[20],_[12],_[12],_[12],_[12],_[21],_[22],_[22],_[22],_[22],_[23],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[24],_[12],_[12],_[12],_[25],_[12],_[26],_[27],_[28],_[12],_[29],_[12],_[30],_[31],_[12],_[32],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],};
transition_actions=_[33];
transition_states=_[34];
accept_actions=_[35];
guard_action=4;
};
{
start_state=132;
max_accept_state=131;
max_state=137;
transitions={[0]=_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[37],_[38],_[37],_[37],_[39],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[37],_[36],_[40],_[41],_[36],_[42],_[43],_[40],_[44],_[45],_[46],_[47],_[48],_[49],_[50],_[51],_[52],_[53],_[53],_[53],_[53],_[53],_[53],_[53],_[53],_[53],_[54],_[55],_[56],_[57],_[58],_[36],_[36],_[59],_[59],_[59],_[59],_[60],_[59],_[61],_[61],_[61],_[61],_[61],_[61],_[61],_[61],_[61],_[62],_[61],_[61],_[61],_[61],_[61],_[61],_[61],_[63],_[61],_[61],_[64],_[36],_[65],_[66],_[61],_[36],_[67],_[68],_[69],_[70],_[71],_[72],_[73],_[74],_[75],_[61],_[76],_[77],_[61],_[78],_[79],_[80],_[61],_[81],_[82],_[83],_[84],_[61],_[85],_[63],_[61],_[61],_[86],_[87],_[88],_[89],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],_[36],};
transition_actions=_[90];
transition_states=_[91];
accept_actions=_[92];
};
action_continuations=_[93];
 }
return setmetatable({}, {
  __index = static_data;
  __call = function (_, source, source_name, eof_symbol, fn)
    return main(static_data, source, source_name, eof_symbol, fn)
  end;
})
