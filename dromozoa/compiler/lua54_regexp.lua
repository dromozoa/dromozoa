local main = function ()
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
local rc
local rd

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
function()ra=0
end;
function()ra=ra*16+fc-0x30
end;
function()ra=ra*16+fc-0x41+10
end;
function()ra=ra*16+fc-0x61+10
end;
function()fassert(ra<=0x7FFFFFFF,'UTF-8 value too large') append_unicode(ra)
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
function()ts=2;clear() fcall(3) push(true)
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
function()ts=nil;fcall(1) push()
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
function()ts=6;push(false,ra,rb+rc*rd)
end;
function()ts=5;push(false, ra)
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
function()ts=1;clear() fcall(2) push(true)
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
function()ra=0 rb=0 rc=1 rd=0
end;
function()ra=ra*16+fc-0x30 rb=rb-4
end;
function()ra=ra*16+fc-0x41+10 rb=rb-4
end;
function()rd=rd*10+fc-0x30
end;
function()rc=-1
end;
function()ra=ra*16+fc-0x61+10 rb=rb-4
end;
 }
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
  local current_byte
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
      ferror "too much recursion; possible loop detected"
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
  function push(value_from_buffer, ...)
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
      ...
    }
  end
  function ferror(message)
    local near = current_byte == nil and "eof" or string.char(current_byte)
    error(source_name .. ":" .. start_line .. ":" .. start_column .. ": regexp error (" .. message .. " near " .. near .. ")")
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
  if false then
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
    if current_byte == nil then
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
    if current_byte == nil then
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
{0,0,0,0,0,0,0,0,0,16,0,0,0,0,0};
{0,0,0,0,5,5,0,0,0,16,0,0,0,0,0};
{0,17,0,20,45,46,0,0,0,16,0,0,0,0,0};
{0,19,18,0,48,47,0,0,0,16,0,0,0,0,0};
{0,21,0,0,0,0,0,0,0,16,0,0,0,0,0};
{0,22,0,0,0,0,23,24,0,16,0,31,32,39,40};
{0,0,0,0,0,0,0,0,0,16,0,36,33,43,41};
{0,21,0,0,0,0,0,0,0,2,0,0,0,0,0};
{0,25,0,0,0,0,0,0,0,16,0,37,34,44,42};
{0,0,0,0,0,0,0,0,0,16,0,37,34,44,42};
{0,26,0,0,0,0,0,0,0,16,0,37,34,44,42};
{0,27,0,0,0,0,0,0,0,16,0,0,0,0,0};
{0,28,0,0,0,0,0,0,0,16,0,0,0,0,0};
{0,29,0,0,0,0,0,0,0,16,0,0,0,0,0};
{0,11,0,0,0,0,0,0,0,16,0,0,0,0,0};
{0,38,0,0,0,0,0,0,0,16,0,0,0,0,0};
{0,14,0,0,0,0,0,0,0,16,0,0,0,0,0};
{0,5,0,0,0,0,0,0,0,16,0,0,0,0,0};
{0,0,0,0,0,0,0,0,0,16,30,0,0,0,0};
{0,0,0,0,0,0,0,0,0,16,0,0,35,0,0};
{5,6,3,6,3,5,8,9,9,10,11,12,13,14,15,16,16,17,18,19,17,18,20,8,21,22,23,24,25,2,2,3,2};
{1,3,1,4,1,1,7,8,9,1,1,1,1,1,12,13,13,13,13,1,13,13,1,15,1,1,1,15,15,6,6,5,5};
{1,1,1,1,1,1,7,7,7};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
{1,1,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0};
{139,140,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,170,0,0,173,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,143,0,0,0,0,0,0,0};
{142,141,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,172,171,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,144,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,145,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13,133,0,0,0,137,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,3,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,133,0,0,0,155,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,18,19,0,0,0,0,0,20,20,0,0,26,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17,0,0,135,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,23,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,22,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,20,0,0,20,21,0,0,25,25,151,154,161,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,24,21,21,160,150,156,153,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,20,0,0,20,21,0,0,25,25,151,154,161,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,25,21,21,160,150,156,153,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,29,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0};
{0,0,4,4,147,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,34,0,0,36,0,38,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,167,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,130,0,35,0,0,0,0,0,0,168};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,39,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,37,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,152,0,162,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,40,0,0,164,158,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,132,0,0,0,132,132,152,0,162,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,40,0,0,164,158,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,40,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,136,0,136,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,40,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,149,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,40,0,0,0,0,0,0,0};
{0,0,146,4,148,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,174,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,166,0,0,0,0,0,0,169};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,102,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,103,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,157,0,163,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,77,40,40,51,40,40,40,87,40,91,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,60,0,0,165,159,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,157,0,163,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,44,0,0,165,159,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,157,0,163,0,0,0,0,0,0,0,0,0,0,0,40,40,45,40,40,40,40,40,40,40,40,40,40,40,68,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,40,0,0,165,159,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,157,0,163,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,104,40,40,40,40,109,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,78,0,0,165,159,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,132,0,0,0,132,132,157,0,163,0,0,0,0,0,0,0,0,0,0,0,40,40,40,47,40,40,40,40,40,40,61,40,40,40,40,70,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,93,40,40,40,40,40,40,107,40,40,110,40,40,40,40,40,40,40,40,40,40,40,123,40,125,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,62,0,0,165,159,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,157,0,163,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,108,40,40,40,40,40,114,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,64,0,0,165,159,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,53,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,59,40,40,40,40,40,40,40,72,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,40,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,66,40,40,40,40,40,40,75,40,40,40,40,40,40,40,40,40,88,40,40,40,40,95,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,80,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,86,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,105,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,40,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,65,40,40,40,40,40,40,40,40,40,40,63,40,40,40,40,40,40,40,40,40,40,40,40,96,40,40,40,40,40,40,40,40,40,40,40,116,117,40,40,40,40,40,40,124,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,46,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40,42,40,40,40,40,40,40,58,40,40,40,40,40,40,40,40,40,40,40,76,40,81,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,112,40,115,40,40,40,40,40,121,122,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,69,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,54,40,40,40,40,40,40,67,40,40,40,40,40,40,40,40,40,40,83,40,84,40,40,89,40,40,40,40,40,40,40,40,106,40,40,40,40,40,40,113,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,90,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,136,0,136,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,55,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,40,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,50,40,40,40,40,40,40,40,40,40,40,40,40,73,40,40,40,40,40,40,40,40,40,40,40,40,40,92,40,40,40,40,40,40,40,40,40,40,40,111,40,40,40,40,40,40,119,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,43,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,79,40,82,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,40,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,52,40,56,40,40,40,40,40,40,40,40,40,40,74,40,40,40,40,40,40,40,40,85,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,118,40,120,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,57,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,71,40,40,40,40,40,40,40,41,40,40,40,40,40,40,40,40,94,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,48,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,0,0,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,0,0,0,0,0,49,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,126,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,127,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0};
{0,0,4,4,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,129,0,0,0,0,0,0,0};
{2,2,3,2,2,2,90,91,92,93,94,95,95,96,97,97,98,97,99,96,99,16,16,17,18,17,18,91,92,92,93,2,3,2,3,93};
{2,2,1,1,2,1,6,5,5,16,134,26,26,26,27,27,137,27,26,26,26,28,28,28,28,28,28,97,138,138,98,99,100,101,100,98};
{26,26,26,26,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,41,42,43,44,44,45,45,46,47,48,49,50,51,52,53,54,55,56,57,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,59,60,60,60,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89};
{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
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
max_state=15;
transitions={[0]=_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[13],_[14],_[13],_[13],_[15],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[13],_[12],_[16],_[12],_[12],_[12],_[12],_[16],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[17],_[17],_[17],_[17],_[17],_[17],_[17],_[17],_[17],_[17],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[18],_[18],_[18],_[18],_[18],_[18],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[19],_[12],_[12],_[12],_[12],_[20],_[21],_[21],_[21],_[21],_[22],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[23],_[12],_[12],_[12],_[24],_[12],_[25],_[26],_[27],_[12],_[28],_[12],_[29],_[30],_[12],_[31],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],_[12],};
transition_actions=_[32];
transition_states=_[33];
accept_actions=_[34];
guard_action=4;
};
{
start_state=131;
max_accept_state=130;
max_state=138;
transitions={[0]=_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[36],_[37],_[36],_[36],_[38],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[36],_[35],_[39],_[40],_[35],_[41],_[42],_[39],_[43],_[44],_[45],_[46],_[47],_[48],_[49],_[50],_[51],_[52],_[52],_[52],_[52],_[52],_[52],_[52],_[52],_[52],_[53],_[54],_[55],_[56],_[57],_[35],_[35],_[58],_[58],_[58],_[58],_[59],_[58],_[60],_[60],_[60],_[60],_[60],_[60],_[60],_[60],_[60],_[61],_[60],_[60],_[60],_[60],_[60],_[60],_[60],_[62],_[60],_[60],_[63],_[35],_[64],_[65],_[60],_[35],_[66],_[67],_[68],_[69],_[70],_[71],_[72],_[73],_[74],_[60],_[75],_[76],_[60],_[77],_[78],_[79],_[60],_[80],_[81],_[82],_[83],_[60],_[84],_[62],_[60],_[60],_[85],_[86],_[87],_[88],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],_[35],};
transition_actions=_[89];
transition_states=_[90];
accept_actions=_[91];
};
action_threads=_[92];
 }
return setmetatable({}, {
  __index = static_data;
  __call = function (_, source, source_name, eof_symbol, fn)
    local thread = coroutine.create(main)
    assert(coroutine.resume(thread))
    return select(2, assert(coroutine.resume(thread, static_data, source, source_name, eof_symbol, fn)))
  end;
})
