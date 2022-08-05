return function (context) return {
[[
local main = function ()
  local fcall
  local fret
  local push
  local clear
  local append
  local tk
  local fs = 1
  local fp
  local fc
  local fb = {}
  local fg = {}
  local ln = 1
  local lp = 0
  ]];
context["custom_data"];
[[

  local action_data = { ]];
context["action_data"];
[[
 }
  local _, source, source_name, fn = coroutine.yield()
  local table_unpack = table.unpack or unpack
  local main = _.main
  local action_threads = _.action_threads
  local start_line = 1
  local start_column = 1
  local current_position = 1
  local current_index = main
  local current_state = _[current_index].start_state
  local current_thread
  local action_type
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
      current_thread = current_thread;
      action_type = action_type;
    }
    if #stack > 2000 then
      error(source_name .. ":" .. start_line .. ":" .. start_column .. ": regexp error (too much recursion; possible loop detected)")
    end
    jumped = true
    tk = nil
    fs = current_position
    start_line = ln
    start_column = fs - lp
    current_index = index
    current_state = _[current_index].start_state
    if current_thread ~= nil then
      current_thread = nil
      coroutine.yield()
    end
  end
  function fret()
    local item = stack[#stack]
    stack[#stack] = nil
    jumped = true
    tk = item.token_symbol
    fs = item.start_position
    start_line = item.start_line
    start_column = item.start_column
    current_index = item.current_index
    current_state = item.current_state
    current_thread = item.current_thread
    if current_thread ~= nil then
      assert(coroutine.resume(current_thread))
    end
    if item.action_type == "accept" then
      tk = nil
      fs = current_position
      start_line = ln
      start_column = fs - lp
      current_state = _[current_index].start_state
    end
  end
  function push(v)
    local source = string.sub(source, fs, fp)
    if v == nil then
      v = source
    elseif type(v) == "table" then
      v = string.char(table_unpack(v))
    end
    fn {
      symbol = tk;
      i = fs;
      j = fp;
      source = source;
      line = start_line;
      column = start_column;
      value = v;
    }
  end
  function clear(buffer)
    buffer = {}
  end
  function append(buffer, v)
    buffer[#buffer + 1] = v
  end
  local function execute(index)
    local action = action_data[index]
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
  local function accept_continuation()
  end
  local function accept(current_byte)
    if current_state > _[current_index].max_accept_state then
      error(source_name .. ":" .. start_line .. ":" .. start_column .. ": regexp error (cannot transition)")
    end
    action_type = "accept"
    if execute(_[current_index].accept_actions[current_state]) then
      action_type = nil
      return
    end
    action_type = nil
    if current_byte == nil then
      if current_index == main then
        fn()
        return true
      end
      return error(source_name .. ":" .. start_line .. ":" .. start_column .. ": regexp error (unexpected eof)")
    end
    if current_state == _[current_index].start_state then
      error(source_name .. ":" .. start_line .. ":" .. start_column .. ": regexp error (loop detected)")
    end
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
    for i = 1, #fg do
      if string.byte(source, current_position + i - 1) ~= fg[i] then
        return transition()
      end
    end
    fp = current_position + #fg - 1
    fc = fg[#fg]
    current_position = current_position + 1
    execute(_[current_index].guard_action)
  end
  repeat until guard()
end
local _ = { ]];
context["shared_data"];
[[
 }
local _ = { ]];
context["static_data"];
[[
 }
return function (source, source_name, fn)
  local thread = coroutine.create(main)
  assert(coroutine.resume(thread))
  assert(coroutine.resume(thread, _, source, source_name, fn))
end
]];
} end
