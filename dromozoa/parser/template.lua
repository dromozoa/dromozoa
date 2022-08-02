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
    create_node
      { [0] = ????, defs... }
    collapse_node
      { [0] = head, defs...}
    default_node
      { [0] = head, reduced_nodes... }

    結果ノードは、
      { [0] = head }
      reduced_nodesのどれか (create_node)
      新規作成ノード

      _[0]
      _[1] .. _[n]
      _"if" => { [0] = symbol }

      node(_[0], _[1], _[3])

      return node(0,1,"if"){
        binop = true
      }

      node {0,1,2,3,4;binop=1}
      node {0,"namelist"}
      node {"namelist"}
      node {[0]=1,"explist";scope=true}

      _0, _1, _2, ...
      create(code) => { [0] = code }
      node:append(node)
      node:set_attribute(k, v)

      return _0:append(_1,_2):attr("binop")



      _[0] { ... }

      _"..." { }

      return create("list")

      _0 { _1, _2, binop=true }

      _[0] { _[1], _[2], binop=true }

      _ = _0 { _1, _2, binop=true }

      name=value

      _(_1, _2("key"))



      _"list"
  ]]

  local _

  local action_data = { $action_data }
  local static_data = coroutine.yield()

  local symbol_names = static_data.symbol_names
  local symbol_table = static_data.symbol_table
  local max_state = static_data.max_state
  local max_terminal_symbol = static_data.max_terminal_symbol
  local max_nonterminal_symbol = static_data.max_nonterminal_symbol
  local actions = _static_dataactions
  local heads = static_data.heads
  local sizes = static_data.sizes
  local semantic_actions = static_actions.semantic_actions

  local stack = { 1 } -- state stack
  local nodes = {}

  while true do
    local token_node = coroutine.yield()
    local symbol = token_node[0]

    while true do
      local state = stack[#stack]
      local action = actions[state][symbol]
      if action == 0 then
        error "???"
      end
      if action <= max_state then
        -- shift
        stack[#stack + 1] = action
        nodes[#nodes + 1] = token_node
        break
      end
      -- reduce or accept
      local index = action - max_state
      if index == 1 then
        -- accept
        local accepted_node = nodes[#nodes]
        stack[#stack] = nil
        nodes[#nodes] = nil
        -- TODO エラーチェック
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

      reduced_nodes[0] = node(head)
      _ = setmetatable(reduced_nodes, metatable)
      action_data[semantic_actions[index]]()

      local state = stack[#stack]
      stack[#stack + 1] = actions[state][head]
      nodes[#nodes + 1] = current_node
    end
  end
end

local _ = { $shared_data }
local static_data = { $static_data }

local metatable = {}

function metatable:__call(token)
  return select(2, assert(coroutine.resume(self.thread, token)))
end

return function ()
  local thread = coroutine.create(main)
  assert(coroutine.resume(thread))
  assert(coroutine.resume(thread, static_data))
  return setmetatable({ thread = thread }, metatable)
end
