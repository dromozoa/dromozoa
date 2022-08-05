return function (context) return {
[[
local main = function ()
  local create
  local append
  local S
  local SS
  ]];
context["custom_data"];
[[

  local action_data = { ]];
context["action_data"];
[=[
 }
  local table_unpack = table.unpack or unpack
  local static_data = coroutine.yield()
  local symbol_names = static_data.symbol_names
  local actions = static_data.actions
  local max_state = #actions
  local heads = static_data.heads
  local sizes = static_data.sizes
  local semantic_actions = static_data.semantic_actions
  local stack = { 1 }
  local nodes = {}
  function create(symbol)
    return { [0] = symbol }
  end
  function append(...)
    for i = 1, select("#", ...) do
      SS[#SS + 1] = select(i, ...)
    end
  end
  while true do
    local token = coroutine.yield()
    local symbol = token[0]
    if symbol ~= nil then
      while true do
        local state = stack[#stack]
        local action = actions[state][symbol]
        if action == 0 then
          local at = ""
          if token.f ~= nil and token.n ~= nil and token.c ~= nil then
            at = token.f .. ":" .. token.n .. ":" .. token.c .. ": "
          end
          error(at .. "parser error (cannot transition near " .. symbol_names[symbol] .. ")")
        end
        if action <= max_state then
          stack[#stack + 1] = action
          nodes[#nodes + 1] = token
          break
        end
        local index = action - max_state
        if index == 1 then
          local accepted_node = nodes[#nodes]
          stack[#stack] = nil
          nodes[#nodes] = nil
          return accepted_node
        end
        local head = heads[index]
        local size = sizes[index]
        S = { [0] = create(head) }
        for i = size - 1, 0, -1 do
          S[#S + 1] = nodes[#nodes - i]
        end
        SS = create(head)
        append(table_unpack(S))
        for i = 1, size do
          stack[#stack] = nil
          nodes[#nodes] = nil
        end
        action_data[semantic_actions[index]]()
        local state = stack[#stack]
        stack[#stack + 1] = actions[state][head]
        nodes[#nodes + 1] = SS
      end
    end
  end
end
local static_data = { ]=];
context["static_data"];
[[
 }
local metatable = {
  __call = function (self, token)
    return select(2, assert(coroutine.resume(self.thread, token)))
  end;
}
return setmetatable({}, {
  __index = static_data;
  __call = function ()
    local thread = coroutine.create(main)
    assert(coroutine.resume(thread))
    assert(coroutine.resume(thread, static_data))
    return setmetatable({ thread = thread }, metatable)
  end;
})
]];
} end
