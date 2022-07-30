return function (context) return {
[[
local _ = { ]];
context.shared_data;
[[
 }
local S = { ]];
context.static_data;
[[
 }
local _

return function (source, source_name, fn)
  local fcall
  local fret
  local push_token
  -- local skip_token
  local clear
  local append

  local fs = 1  -- start position
  local fp      -- current position
  local fc      -- current character
  local fb = {} -- string buffer
  local fg = {} -- guard buffer
  local ln = 1  -- line number
  local lp = 0  -- line position
  local tk      -- token symbol

  local _ = (function ()
    local S
    local source
    local source_name
    local fn
    ]];
context.custom_data;
[[

    return { ]];
context.action_data;
[[
 }
  end)()
  local _ = { ]];
context.merged_data;
[[
 }

  local table_unpack = table.unpack or unpack

  local start_line = 1
  local start_column = 1
  local current_position = 1
  local current_index = _.main
  local current_state = _[current_index].start_state
  local current_loop

  local stack = {}
  local jumped = false

  function fcall(index)
    stack[#stack + 1] = {
      token_symbol = tk;
      start_position = fs;
      start_line = start_line;
      start_column = start_column;
      current_index = current_index;
      current_state = current_state;
    }

    tk = nil
    fs = current_position
    start_line = ln
    start_column = fs - lp
    current_index = index
    current_state = _[current_index].start_state

    jumped = true
    coroutine.yield()
  end

  function fret()
    local item = stack[#stack]
    stack[#stack] = nil

    local thread = assert(item.thread)
    tk = item.token_symbol
    fs = item.start_position
    start_line = item.start_line
    start_column = item.start_column
    current_index = item.current_index
    current_state = item.current_state

    jumped = true
    assert(coroutine.resume(thread))
  end

  function push_token(value)
    local source = string.sub(source, fs, fp)
    if value == nil then
      value = source
    elseif type(value) == "table" then
      value = string.char(table_unpack(value))
    end
    fn {
      symbol = tk;
      i = fs;
      j = fp;
      source = source;
      line = start_line;
      column = start_column;
      value = value;
    }
  end

  function clear(buffer)
    buffer = {}
  end

  function append(buffer, value)
    buffer[#buffer + 1] = value
  end

  local function execute(action)
    -- TODO コルーチンを作るかどうか前もってわかるはず。
    jumped = false
    local thread = coroutine.create(action)
    assert(coroutine.resume(thread))
    if coroutine.status(thread) == "suspended" then
      stack[#stack].thread = thread
    end
    return jumped
  end

  local function guard(current_byte)
    if _[current_index].guard_action ~= nil and current_state == _[current_index].start_state then
      -- TODO あとで効率化する
      local guard = string.char(table_unpack(fg))
      local p = current_position + #guard - 1
      if string.sub(source, current_position, p) == guard then
        current_position = p + 1
        fp = p
        fc = string.byte(source, p)
        execute(_[current_index].guard_action)
        return true
      end
    end
  end

  local function transition(current_byte, s)
    fp = current_position
    fc = current_byte
    if s > _[current_index].max_state then
      local t = s - _[current_index].max_state
      current_position = current_position + 1
      current_state = _[current_index].transition_states[t]
      execute(_[current_index].transition_actions[t])
    else
      current_position = current_position + 1
      current_state = s
    end
    return true
  end

  local function accept(current_byte)
    if current_state <= _[current_index].max_accept_state then
      if execute(_[current_index].accept_actions[current_state]) then
        return
      end
      if current_byte == nil then
        if current_index == _.main then
          -- push eof
          fn()
          return true
        end
        error(source_name .. ":" .. start_line .. ":" .. start_column .. ": unexpected eof")
      else
        -- 初期状態でなければ、再度を実行する。
        if current_state ~= _[current_index].start_state then
          tk = nil
          fs = current_position
          start_line = ln
          start_column = fs - lp
          current_state = _[current_index].start_state
          return
        end
      end
    end
    error(source_name .. ":" .. start_line .. ":" .. start_column .. ": regexp error")
  end

  while true do
    local current_byte = string.byte(source, current_position)
    if not guard(current_byte) then
      local s = 0
      if current_byte ~= nil then
        s = _[current_index].transitions[current_byte][current_state]
      end
      if s ~= 0 then
        transition(current_byte, s)
      else
        if accept(current_byte) then
          break
        end
      end
    end
  end
end
]];
} end
