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

local main = function ()
  --[[
    -- 初期化
    $0 = create() -- headで作成する
    $$ = $0

    -- デフォルトのセットアップ
    $$.append($1, $2, ...)

    -- collapse
    $$ = $1
    $$ = create($E) -- 非終端シンボルで空のノードを作成

    -- 子ノードを設定する
    $$.append(...)

    -- 属性を設定する
    $$.attr = $1.attr + $2.attr

    $1.unop = true

    検討項目: $を使うかどうか。
    $を_で置き換えれば自然なLuaコードになる
    $をSで置き換えるのはどうか。
    symbolかな？

    S[0] = create()
    S[1]


  ]]

  local _

  $custom_data
  local action_data = { $action_data }
  local static_data, source_name = coroutine.yield()

  local actions = static_data.actions
  local max_state = #actions
  local heads = static_data.heads
  local sizes = static_data.sizes
  local semantic_actions = static_data.semantic_actions

  local stack = { 1 }
  local nodes = {}

  while true do
    local token_node = coroutine.yield()
    local symbol = token_node[0]
    if symbol ~= nil then
      while true do
        local state = stack[#stack]
        local action = actions[state][symbol]
        if action == 0 then
          error(source_name .. ":" .. token_node.n .. ":" .. token_node.c .. ": parser error (cannot transition)")
        end

        -- shift
        if action <= max_state then
          stack[#stack + 1] = action
          nodes[#nodes + 1] = token_node
          break
        end

        local index = action - max_state
        -- accept
        if index == 1 then
          local accepted_node = nodes[#nodes]
          stack[#stack] = nil
          nodes[#nodes] = nil
          return accepted_node
        end

        -- reduce
        local head = heads[index]
        local size = sizes[index]

        local reduced_nodes = {}
        for i = size - 1, 0, -1 do
          reduced_nodes[#reduced_nodes + 1] = nodes[#nodes - i]
        end
        for i = 1, size do
          stack[#stack] = nil
          nodes[#nodes] = nil
        end

        reduced_nodes[0] = head
        _ = { [0] = reduced_nodes }
        for i = 1, #reduced_nodes do
          _[i] = reduced_nodes[i]
        end
        action_data[semantic_actions[index]]()

        local state = stack[#stack]
        stack[#stack + 1] = actions[state][head]
        nodes[#nodes + 1] = _[0]
      end
    end
  end
end

local static_data = { $static_data }

local metatable = {
  __call = function (self, token)
    return select(2, assert(coroutine.resume(self.thread, token)))
  end;
}

return setmetatable({}, {
  __index = static_data;
  __call = function (_, source_name)
    local thread = coroutine.create(main)
    assert(coroutine.resume(thread))
    assert(coroutine.resume(thread, static_data, source_name))
    return setmetatable({ thread = thread }, metatable)
  end;
})
