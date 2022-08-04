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

local array = require "dromozoa.array"
local tree_set = require "dromozoa.tree_set"
local runtime = require "dromozoa.regexp.runtime"

local function insert_action(action_ctx, action)
  -- local action = action:gsub("$([%a_][%w%_]*)", function (name)
  --   local result = context[name]
  --   if result == nil then
  --     error("name " .. name .. " not defined")
  --   end
  --   return result
  -- end):gsub([[${([^%s<>\]*)<(..-)>%1}]], function (_, s)
  --   local buffer = {}
  --   for i, v in ipairs { s:byte(1, #s) } do
  --     buffer[i] = ("0x%X"):format(v)
  --   end
  --   return table.concat(buffer, ",")
  -- end)

  return (select(2, action_ctx.set:insert(action)))
end

local function update_state_indices_accept(u, action_ctx, accept_actions, color)
  color[u] = 1
  if u.accept_action ~= nil then
    u.index = accept_actions:append(insert_action(action_ctx, u.accept_action)):size()
  end
  for _, t in u.transitions:ipairs() do
    if color[t.v] == nil then
      update_state_indices_accept(t.v, action_ctx, accept_actions, color)
    end
  end
  color[u] = 2
end

local function update_state_indices_nonaccept(u, index, color)
  color[u] = 1
  if u.accept_action == nil then
    index = index + 1
    u.index = index
  end
  for _, t in u.transitions:ipairs() do
    if color[t.v] == nil then
      index = update_state_indices_nonaccept(t.v, index, color)
    end
  end
  color[u] = 2
  return index
end

local function construct_table(u, max_state, action_ctx, transitions, transition_actions, transition_states, color)
  color[u] = 1
  for _, t in u.transitions:ipairs() do
    local v = t.v.index
    if t.action ~= nil then
      v = max_state + transition_actions:append(insert_action(action_ctx, t.action)):size()
      transition_states:append(t.v.index)
    end
    for byte in pairs(t.set) do
      transitions:get(byte + 1):set(u.index, v)
    end
    if color[t.v] == nil then
      construct_table(t.v, max_state, action_ctx, transitions, transition_actions, transition_states, color)
    end
  end
  color[u] = 2
end

local function generate(index, u, guard_action, static_out, shared_set, action_ctx)
  local accept_actions = array()
  update_state_indices_accept(u, action_ctx, accept_actions, {})
  local max_state = update_state_indices_nonaccept(u, accept_actions:size(), {})

  local transitions = array()
  for i = 1, 256 do
    transitions:append(array.fill(max_state, 0))
  end
  local transition_actions = array()
  local transition_states = array()

  construct_table(u, max_state, action_ctx, transitions, transition_actions, transition_states, {})

  static_out:append(
    "{\n",
    "start_state=", u.index, ";\n",
    "max_accept_state=", accept_actions:size(), ";\n",
    "max_state=", max_state, ";\n",
    "transitions={[0]=")
  for _, v in transitions:ipairs() do
    static_out:append("_[", select(2, shared_set:insert(v)), "],")
  end
  static_out:append(
    "};\n",
    "transition_actions=_[", select(2, shared_set:insert(transition_actions)), "];\n",
    "transition_states=_[", select(2, shared_set:insert(transition_states)), "];\n",
    "accept_actions=_[", select(2, shared_set:insert(accept_actions)), "];\n")
  if guard_action ~= nil then
    static_out:append("guard_action=", insert_action(action_ctx, guard_action), ";\n")
  end
  static_out:append(
    "};\n")
end

return function (that)
  local data = array()
  local custom_out = array()
  for k, v in pairs(that) do
    if type(k) == "string" then
      data:append { timestamp = v.timestamp, machine = v, name = k }
    end
  end
  local j = 0
  for i, v in ipairs(that) do
    if type(v) == "string" then
      custom_out:append(v, "\n")
    else
      j = j + 1
      data:append { timestamp = v.timestamp, machine = v, main = j == 1 }
    end
  end
  data:sort(function (a, b) return a.timestamp < b.timestamp end)

  local action_ctx = { set = tree_set(), variables = {}, threads = array() }
  for i, v in data:ipairs() do
    if v.name ~= nil then
      action_ctx.variables[v.name] = i
    end
  end

  local static_out = array()
  local shared_set = tree_set()
  -- local action_set = tree_set()
  for i, v in data:ipairs() do
    if v.main then
      static_out:append("main=", i, ";\n")
    end
    generate(i, v.machine.start_state, v.machine.guard_action, static_out, shared_set, action_ctx)
  end

  local action_out = array()
  for _, v in action_ctx.set:ipairs() do
    -- TODO 展開後のコードが同じ文字列になることがあるかもしれない
    -- TODO 展開後のコードでコルーチンの必要性判定を行うべき？
    action_out:append("function()", v:gsub("$([%a_][%w%_]*)", function (name)
      local result = action_ctx.variables[name]
      if result == nil then
        error("name " .. name .. " not defined")
      end
      return result
    end):gsub([[${([^%s<>\]*)<(..-)>%1}]], function (_, s)
      local buffer = {}
      for i, v in ipairs { s:byte(1, #s) } do
        buffer[i] = ("0x%X"):format(v)
      end
      return table.concat(buffer, ",")
    end), "\nend;\n")

    -- コルーチンの必要性をおおまかに検査する。
    -- 1. 単語境界を調べやすくするために番兵を置く。
    local s = " " .. v .. " "
    -- 2. fcallという単語が最初に出現する位置を調べる。
    local p = s:find "[^%w_](fcall)[^%w_]"
    -- 3. fcallという単語が最後に出現する位置を調べる。
    local q = s:find "[^%w_](fcall)%s*%b()%s*$"
    if p == q then
      action_ctx.threads:append(0)
    else
      action_ctx.threads:append(1)
    end
  end
  static_out:append("action_threads=_[", select(2, shared_set:insert(action_ctx.threads)), "];\n")

  local shared_out = array()
  for _, v in shared_set:ipairs() do
    shared_out:append("{", v:concat ",", "};\n")
  end

  return table.concat(runtime {
    shared_data = shared_out:concat();
    static_data = static_out:concat();
    custom_data = custom_out:concat();
    action_data = action_out:concat();
  })
end
