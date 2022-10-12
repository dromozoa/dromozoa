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
function()ra=fc-48
end;
function()ra=ra*10+fc-48
end;
function()append(0x07)
end;
function()append(0x08)
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
function()rb=rb*16+fc-48
end;
function()rb=rb*16+fc-65+10
end;
function()rb=rb*16+fc-97+10
end;
function()fassert(fp-ra<=9 and rb<=0x7FFFFFFF,'UTF-8 value too large') append_unicode(rb)
end;
function()append(0x0B)
end;
function()append(ra*16+fc-48)
end;
function()append(ra*16+fc-65+10)
end;
function()append(ra*16+fc-97+10)
end;
function()ra=fc-65+10
end;
function()ra=fc-97+10
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
function()guard_clear(93)
end;
function()guard_append(fc)
end;
function()guard_append(93)
end;
 }
  end)()
  local error = error
  local select = select
  local string_byte = string.byte
  local string_char = string.char
  local string_sub = string.sub
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
    local s = string_sub(source, fs, fp)
    local v = s
    if value_from_buffer then
      v = string_char(table_unpack(buffer))
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
      error(source_name .. ":" .. start_line .. ":" .. start_column .. ": regexp error (" .. message .. " near '" .. string_char(current_byte) .. "')")
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
    append(string_byte(source, i, j))
  end
  if utf8 and utf8.char then
    function append_unicode(a)
      append(string_byte(utf8.char(a), 1, -1))
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
    guard_append(string_byte(source, i, j))
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
    current_byte = string_byte(source, current_position)
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
      if string_byte(source, current_position + i - 1) ~= guard_buffer[i] then
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
{0,0,0,0,0,0,0,0,17,0,0,0,0,0,0,0};
{0,0,0,4,4,0,0,0,17,0,0,0,0,0,0,0};
{0,0,21,53,54,0,0,0,17,18,0,0,0,0,0,0};
{0,19,0,56,55,0,0,0,17,20,0,0,0,0,0,0};
{0,0,0,0,0,0,0,0,17,22,0,0,0,0,0,0};
{0,0,0,0,0,24,25,0,17,23,0,33,34,36,47,48};
{0,0,0,0,0,24,25,0,17,23,0,43,35,36,47,48};
{0,0,0,0,0,0,0,0,17,0,0,44,40,37,51,49};
{0,0,0,0,0,0,0,0,10,22,0,0,0,0,0,0};
{0,0,0,0,0,0,0,0,17,26,0,45,41,38,52,50};
{0,0,0,0,0,0,0,0,17,27,0,45,41,38,52,50};
{0,0,0,0,0,0,0,0,17,0,0,45,41,38,52,50};
{0,0,0,0,0,0,0,0,17,28,0,45,41,38,52,50};
{0,0,0,0,0,0,0,0,17,29,0,0,0,0,0,0};
{0,0,0,0,0,0,0,0,17,30,0,0,0,0,0,0};
{0,0,0,0,0,0,0,0,17,31,0,0,0,0,0,0};
{0,0,0,0,0,0,0,0,17,11,0,0,0,0,0,0};
{0,0,0,0,0,0,0,0,17,46,0,0,0,0,0,0};
{0,0,0,0,0,0,0,0,17,15,0,0,0,0,0,0};
{0,0,0,0,0,0,0,0,17,4,0,0,0,0,0,0};
{0,0,0,0,0,0,0,0,17,0,32,0,0,0,0,0};
{0,0,0,0,0,0,0,0,17,0,0,0,42,39,0,0};
{5,6,3,6,3,5,8,9,9,10,11,12,13,14,15,16,17,17,18,18,19,20,21,19,20,21,18,19,20,22,8,23,24,25,26,27,2,2,3,2};
{1,2,1,3,1,1,6,7,8,1,1,1,1,1,1,12,13,13,14,14,14,14,1,14,14,1,14,14,14,1,16,1,1,1,16,16,5,5,4,4};
{1,1,1,1,1,7,7,7};
{0,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{2,2,2,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{142,138,139,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,152,0,0,155,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{143,141,140,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,154,153,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{144,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{8,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{9,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{10,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{11,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{12,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{13,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{14,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,29,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,134,0,0,0,0};
{15,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{16,0,0,5,5,5,0,0,0,0,0,0,0,0,0,4,5,0,0,0,0,0,0,0,0,0,0,29,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,134,0,0,0,0};
{18,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,19,20,0,0,0,0,0,21,21,0,0,0,27,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,136,0,0};
{23,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{25,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,21,0,0,21,22,0,0,26,26,27,29,29,30,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,22,22,30,27,0};
{26,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,21,0,0,21,22,0,0,26,26,27,29,29,30,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,22,22,30,27,0};
{31,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{33,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{34,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,35,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{37,0,0,5,5,146,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,36,0,0,38,0,40,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,149,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,132,0,0,0,0,0,150};
{39,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{42,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,27,0,0,30,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,30,27,0};
{42,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,133,0,0,0,133,133,27,0,0,30,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,30,27,0};
{42,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,0,0,0};
{42,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,28,0,0,28,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,0,0,0};
{42,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,135,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,0,0,0};
{148,0,0,145,5,147,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,156,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,151};
{104,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{105,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{62,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,27,0,0,30,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,79,42,42,53,42,42,42,89,42,93,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,30,27,0};
{46,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,27,0,0,30,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,30,27,0};
{42,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,27,0,0,30,0,0,0,0,0,0,0,0,0,0,0,42,42,47,42,42,42,42,42,42,42,42,42,42,42,70,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,30,27,0};
{80,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,27,0,0,30,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,106,42,42,42,42,111,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,30,27,0};
{64,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,133,0,0,0,133,133,27,0,0,30,0,0,0,0,0,0,0,0,0,0,0,42,42,42,49,42,42,42,42,42,42,63,42,42,42,42,72,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,95,42,42,42,42,42,42,109,42,42,112,42,42,42,42,42,42,42,42,42,42,42,125,42,127,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,30,27,0};
{66,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,27,0,0,30,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,110,42,42,42,42,42,116,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,30,27,0};
{55,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,0,0,0};
{42,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,61,42,42,42,42,42,42,42,74,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,0,0,0};
{88,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,68,42,42,42,42,42,42,77,42,42,42,42,42,42,42,42,42,90,42,42,42,42,97,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,82,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,0,0,0};
{42,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,107,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,0,0,0};
{48,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,67,42,42,42,42,42,42,42,42,42,42,65,42,42,42,42,42,42,42,42,42,42,42,42,98,42,42,42,42,42,42,42,42,42,42,42,118,119,42,42,42,42,42,42,126,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,0,0,0};
{71,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,42,44,42,42,42,42,42,42,60,42,42,42,42,42,42,42,42,42,42,42,78,42,83,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,114,42,117,42,42,42,42,42,123,124,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,0,0,0};
{92,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,56,42,42,42,42,42,42,69,42,42,42,42,42,42,42,42,42,42,85,42,86,42,42,91,42,42,42,42,42,42,42,42,108,42,42,42,42,42,42,115,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,0,0,0};
{42,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,28,0,0,28,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,57,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,0,0,0};
{45,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,52,42,42,42,42,42,42,42,42,42,42,42,42,75,42,42,42,42,42,42,42,42,42,42,42,42,42,94,42,42,42,42,42,42,42,42,42,42,42,113,42,42,42,42,42,42,121,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,0,0,0};
{42,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,81,42,84,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,0,0,0};
{59,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,54,42,58,42,42,42,42,42,42,42,42,42,42,76,42,42,42,42,42,42,42,42,87,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,120,42,122,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,0,0,0};
{50,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,73,42,42,42,42,42,42,42,43,42,42,42,42,42,42,42,42,96,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,0,0,0};
{51,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,0,0,0};
{128,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{129,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{130,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{131,0,0,5,5,5,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{2,2,3,2,2,2,94,95,96,97,95,96,96,97,2,3,2,3,97};
{3,3,2,2,3,2,7,6,6,17,99,137,137,100,101,102,103,102,100};
{28,28,28,28,28,28,29,31,32,33,34,35,36,37,38,39,40,42,43,44,45,45,46,47,48,48,49,49,49,50,51,52,53,54,55,56,57,58,59,60,61,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,63,64,64,64,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93};
{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,41,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
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
start_state=9;
max_accept_state=8;
max_state=16;
transitions={[0]=_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[13],_[14],_[13],_[13],_[15],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[13],_[12],_[16],_[12],_[12],_[12],_[12],_[16],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[17],_[18],_[18],_[18],_[18],_[18],_[18],_[18],_[18],_[18],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[19],_[19],_[19],_[19],_[19],_[19],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[20],_[12],_[12],_[12],_[12],_[21],_[22],_[23],_[23],_[23],_[24],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[25],_[12],_[12],_[12],_[26],_[12],_[27],_[28],_[29],_[12],_[30],_[12],_[31],_[32],_[12],_[33],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],};
transition_actions=_[34];
transition_states=_[35];
accept_actions=_[36];
guard_action=4;
};
{
start_state=1;
max_accept_state=132;
max_state=137;
transitions={[0]=_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[38],_[39],_[38],_[38],_[40],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[38],_[37],_[41],_[42],_[37],_[43],_[44],_[41],_[45],_[46],_[47],_[48],_[49],_[50],_[51],_[52],_[53],_[54],_[54],_[54],_[54],_[54],_[54],_[54],_[54],_[54],_[55],_[56],_[57],_[58],_[59],_[37],_[37],_[60],_[60],_[60],_[60],_[61],_[60],_[62],_[62],_[62],_[62],_[62],_[62],_[62],_[62],_[62],_[63],_[62],_[62],_[62],_[62],_[62],_[62],_[62],_[64],_[62],_[62],_[65],_[37],_[66],_[67],_[62],_[37],_[68],_[69],_[70],_[71],_[72],_[73],_[74],_[75],_[76],_[62],_[77],_[78],_[62],_[79],_[80],_[81],_[62],_[82],_[83],_[84],_[85],_[62],_[86],_[64],_[62],_[62],_[87],_[88],_[89],_[90],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],_[37],};
transition_actions=_[91];
transition_states=_[92];
accept_actions=_[93];
};
action_continuations=_[94];
 }
return setmetatable({}, {
  __index = static_data;
  __call = function (_, source, source_name, eof_symbol, fn)
    return main(static_data, source, source_name, eof_symbol, fn)
  end;
})
