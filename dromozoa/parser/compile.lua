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

local tree_set = require "dromozoa.tree_set"
local runtime = require "dromozoa.parser.runtime"

local function append(t, ...)
  local n = #t
  for i = 1, select("#", ...) do
    local v = select(i, ...)
    assert(v ~= nil)
    t[n + i] = v
  end
end

return function (grammar, actions)
  local static_data = {}
  local action_data = tree_set()

  append(static_data, "symbol_names={")
  for i, v in grammar.symbol_names:ipairs() do
    append(static_data, ("%q,"):format(v))
  end
  append(static_data, "};\n", "max_terminal_symbol=", grammar.max_terminal_symbol, ";\n", "actions={\n")
  for _, action in ipairs(actions) do
    append(static_data, "{", table.concat(action, ","), "};\n")
  end
  append(static_data, "};\n", "heads={")
  for _, production in grammar.productions:ipairs() do
    append(static_data, production.head, ",")
  end
  append(static_data, "};\n", "sizes={")
  for _, production in grammar.productions:ipairs() do
    append(static_data, production.body:size(), ",")
  end
  append(static_data, "};\n", "semantic_actions={")

  local function substitute(variable)
    local result = grammar.symbol_table[variable]
    if result == nil then
      error("variable " .. variable .. " not defined")
    end
    return result
  end

  for i, production in grammar.productions:ipairs() do
    local semantic_action = production.semantic_action
    if semantic_action == nil then
      semantic_action = ""
    end
    local semantic_action = semantic_action
      :gsub("$([%a_][%w_]*)", substitute)
      :gsub([[${'(..-)'}]], substitute)
      :gsub("$([1-9]%d*)", "S[%1]")
      :gsub("$0", "S[0]")
      :gsub("$%$", "SS")
    append(static_data, select(2, action_data:insert("function ()" .. semantic_action .. "\nend;\n")), ",")
  end
  append(static_data, "};\n")

  return table.concat(runtime {
    custom_data = grammar.custom_data:concat();
    action_data = action_data:concat();
    static_data = table.concat(static_data);
  })
end
