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
  local ra
  local rb
  local rc
  local rd

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

    coroutine.yield()
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

    local rep = true
    while rep do
      local s = 0
      if current_byte ~= nil then
        s = _[current_index].transitions[current_byte][current_state]
      end

      repeat
        if s == 0 then
          local error_message
          if current_state <= _[current_index].max_accept_state then
            local save_current_index = current_index
            local save_current_state = current_state

            local thread = coroutine.create(_[current_index].accept_actions[current_state])
            assert(coroutine.resume(thread))
            -- fcallされたので、現在の設定で確認をする
            if coroutine.status(thread) == "suspended" then
              stack[#stack].thread = thread
              break
            end

            -- fretされたら、その状態で、もっかい実行する
            -- fretはgoto(tailcall)とみなすべき？
            if current_index ~= save_current_index or current_state ~= save_current_state then
              break
            end

            if current_byte == nil then
              if current_index == main then
                return fn()
              end
              error_message = "unexpected eof"
            else
              -- zero文字マッチの場合、ループする可能性がある
              if current_state ~= _[current_index].start_state then
                fs = current_position
                start_line = ln
                start_column = fs - lp
                current_state = _[current_index].start_state
                break
              end
            end
          end
          if error_message == nil then
            error_message = "regexp error"
          end
          error(source_name .. ":" .. start_line .. ":" .. start_column .. ": " .. error_message)
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

          rep = false
          break
        end
      until true
    end

    ----------------------------------------------------------------------

  end

end
]=];
} end
