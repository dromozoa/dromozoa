return function (context) return {
[[
local _ = { ]];
context.shared_data;
[[
 }
local static_data = { ]];
context.static_data;
[[
 }
local _

return function (source, source_name, fn)
  local fcall
  local fret
  local push_token
  local skip_token
  local clear
  local append

  local fs = 1  -- start position
  local fp      -- current position
  local fc      -- current character
  local fb = {} -- string buffer
  local fg = {} -- guard buffer
  local ln = 1  -- line number
  local lp = 0  -- line position
  local token_symbol

  -- TODO カスタム初期化ルーチン
  -- local ra
  -- local rb
  -- local rc
  -- local rd

  local _ = (function ()
    local static_data
    local source
    local source_name
    local fn
    return { ]];
context.action_data;
[=[
 }
  end)()

  for i, u in ipairs(static_data) do
    for k, v in pairs(u) do
      _[i][k] = v
    end
  end
  local main = static_data.main
  local machines = static_data.machines

  local start_line = 1
  local start_column = 1
  local current_position = 1
  local current_byte
  local current_index = main
  local current_state = _[current_index].start_state
  local current_loop

  local stack = {}

--[[
  local function save_stack()
    return {
      start_position = fs;
      start_line = start_line;
      start_column = start_column;
      current_index = current_index;
      current_state = current_state;
    }
  end

  local function restore_stack(item)
    fs = item.start_position
    start_line = item.start_line
    start_column = item.start_column
    current_index = item.current_index
    current_state = item.current_state
  end

  local function prepare_stack(index)
    fs = current_position
    start_line = ln
    start_column = fs - lp
    current_index = index
    current_state = _[current_index].start_state
  end
]]

  function fcall(name)
    local index = assert(machines[name])

    stack[#stack + 1] = {
      start_position = fs;
      start_line = start_line;
      start_column = start_column;
      current_index = current_index;
      current_state = current_state;
    }

    fs = current_position
    start_line = ln
    start_column = fs - lp
    current_index = index
    current_state = _[current_index].start_state

    coroutine.yield("fcall", index)
  end

  function fret()
    local item = stack[#stack]
    stack[#stack] = nil

    local thread = assert(item.thread)
    fs = item.start_position
    start_line = item.start_line
    start_column = item.start_column
    current_index = item.current_index
    current_state = item.current_state
    assert(coroutine.resume(thread))
  end

  function push_token(value)
    local source = string.sub(source, fs, fp)
    if value == nil then
      value = source
    elseif type(value) == "table" then
      value = string.char(table.unpack(value))
    end
    fn {
      symbol = token_symbol;
      i = fs;
      j = fp;
      source = source;
      line = start_line;
      column = start_column;
      value = value;
    }
  end

  function skip_token()
    fn {
      i = fs;
      j = fp;
      source = string.sub(source, fs, fp);
      line = start_line;
      column = start_column;
    }
  end

  function clear(buffer)
    buffer = {}
  end

  function append(buffer, value)
    buffer[#buffer + 1] = value
  end

  while true do
    current_byte = string.byte(source, current_position)

    local s = 0
    if current_byte ~= nil then
      s = _[current_index].transitions[current_byte][current_state]
    end

    if s == 0 then
      local error_message

--[[
      while current_state <= _[current_index].max_state do
        local thread = coroutine.create(_[current_index].accept_actions[current_state])
        local _, command, index = assert(coroutine.resume(thread))
        if command == "fcall" then
          assert(coroutine.status(thread) == "suspended")
          local item = save_stack()
          item.thread = thread
          stack[#stack + 1] = item
          prepare_stack(index)
          -- fretしてから、この続きをやるべき
          break
        elseif command == "fret" then
          assert(coroutine.status(thread) == "suspended")
          -- 現在のスタックを保存して、戻り先のスレッドを呼び出す
          local item1 = save_stack()
          local item2 = stack[#stack]
          stack[#stack] = nil
          local thread2 = assert(item2.thread)
          restore_stack(item2)
          assert(coroutine.resume(thread2))
          -- 本来はn段階で処理するべき。再帰する？
          assert(coroutine.status(thread2) == "dead")
          -- 現在のスタックを戻す
          -- restore_stack(item1)
        else
          assert(coroutine.status(thread) == "dead")
        end
      end
]]

      if current_state <= _[current_index].max_accept_state then
        local thread = coroutine.create(_[current_index].accept_actions[current_state])
        assert(coroutine.resume(thread))
        if coroutine.status(thread) == "suspended" then
          stack[#stack].thread = thread
        end

        -- current_stateがfretで変わっているかもしれない。

        if current_byte == nil then
          if current_index == main then
            return fn()
          else
            error_message = "unexpected eof"
          end
        else
          -- loopする
          -- fgoto(current_index)
          -- 現在のマシンを再度実行する
          fs = current_position
          start_line = ln
          start_column = fs - lp
          current_state = _[current_index].start_state
        end

      else
        error_message = "regexp error"
      end

      if error_message ~= nil then
        error(source_name .. ":" .. start_line .. ":" .. start_column .. ": " .. error_message)
      end
    else
      fp = current_position
      fc = current_byte
      if s > _[current_index].max_state then
        local t = s - _[current_index].max_state
        current_position = current_position + 1
        current_state = _[current_index].transition_states[t]
        local thread = coroutine.create(_[current_index].transition_actions[t])
        assert(coroutine.resume(thread))
        if coroutine.status(thread) == "suspended" then
          stack[#stack].thread = thread
        end
      else
        current_position = current_position + 1
        current_state = s
      end
    end
  end

end
]=];
} end
