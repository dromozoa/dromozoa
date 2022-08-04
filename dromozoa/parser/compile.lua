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
local runtime = require "dromozoa.parser.runtime"

return function (grammar, actions)
  local static_data = array()
  local action_data = tree_set()

  local symbol_names = grammar.symbol_names
  local max_terminal_symbol = grammar.max_terminal_symbol
  local productions = grammar.productions

  static_data:append(
    "symbol_names={")
  for _, v in symbol_names:ipairs() do
    static_data:append(("%q,"):format(v))
  end
  static_data:append(
    "};\n",
    "max_terminal_symbol=", max_terminal_symbol, ";\n",
    "actions={\n")
  for _, action in ipairs(actions) do
    static_data:append("{", table.concat(action, ","), "};\n")
  end
  static_data:append(
    "};\n",
    "heads={")
  for _, production in productions:ipairs() do
    static_data:append(production.head, ",")
  end
  static_data:append(
    "};\n",
    "sizes={")
  for _, production in productions:ipairs() do
    static_data:append(production.body:size(), ",")
  end
  static_data:append(
    "};\n",
    "semantic_actions={")
  for i, production in productions:ipairs() do
    local semantic_action = production.semantic_action
    if semantic_action == nil then
      semantic_action = ""
    end
    static_data:append(select(2, action_data:insert("function ()" .. semantic_action .. "\nend;\n")), ",")
  end
  static_data:append(
    "};\n")

  return table.concat(runtime {
    action_data = action_data:concat();
    static_data = static_data:concat();
  })
end
