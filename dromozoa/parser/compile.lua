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

local list = require "dromozoa.list"
local tree_set = require "dromozoa.tree_set"
local runtime = require "dromozoa.parser.runtime"

return function (that)
  local shared_set = tree_set()
  local shared_data = list()
  local static_data = list()
  local action_set = tree_set()
  local action_data = list()

  static_data:append(
    "max_state=", that.max_state, ";\n",
    "max_terminal_symbol=", that.max_terminal_symbol, ";\n",
    "actions={")
    -- max_nonterminal_symbolは必要？

  for _, action in ipairs(that.actions) do
    static_data:append("_[", select(2, shared_set:insert(action)), "],")
  end

  static_data:append(
    "};\n",
    "heads=_[", select(2, shared_set:insert(that.heads)), "];\n",
    "sizes=_[", select(2, shared_set:insert(that.sizes)), "];\n",
    "semantic_actions={")

  for _, semantic_action in ipairs(that.semantic_actions) do
    static_data:append(select(2, action_set:insert(semantic_action)), ",")
  end

  static_data:append(
    "};\n")

  for _, v in shared_set:ipairs() do
    shared_data:append("{", table.concat(v, ","), "};\n")
  end

  for _, v in action_set:ipairs() do
    action_data:append("function ()", v, "\nend;\n")
  end

  return table.concat(runtime {
    action_data = table.concat(action_data);
    shared_data = table.concat(shared_data);
    static_data = table.concat(static_data);
  })
end
