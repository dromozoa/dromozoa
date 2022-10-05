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

local append = require "dromozoa.append"
local quote_lua = require "dromozoa.quote_lua"
local runtime = require "dromozoa.parser.runtime"

return function (grammar, actions)
  local static_data = {}
  local action_map = {}
  local action_set = {}

  append(static_data, "symbol_names={")
  for i, v in ipairs(grammar.symbol_names) do
    append(static_data, quote_lua(v), ",")
  end
  append(static_data, "};\nmax_terminal_symbol=", grammar.max_terminal_symbol, ";\nactions={\n")
  for _, action in ipairs(actions) do
    append(static_data, "{", table.concat(action, ","), "};\n")
  end
  append(static_data, "};\nheads={")
  for _, production in ipairs(grammar.productions) do
    append(static_data, production.head, ",")
  end
  append(static_data, "};\nsizes={")
  for _, production in ipairs(grammar.productions) do
    append(static_data, #production.body, ",")
  end
  append(static_data, "};\nsemantic_actions={")

  for i, production in ipairs(grammar.productions) do
    local semantic_action = production.semantic_action
    if not semantic_action then
      semantic_action = ""
    end
    local semantic_action = semantic_action
      :gsub("%$([%a_][%w_]*)", grammar.symbol_table)
      :gsub("%$%{%'(..-)%'%}", grammar.symbol_table)
      :gsub("%$([1-9]%d*)", "S[%1]")
      :gsub("%$0", "S[0]")
      :gsub("%$%$", "SS")

    local v = "function()" .. semantic_action .. "\nend;\n"
    local n = action_map[v]
    if not n then
      n = #action_set + 1
      action_map[v] = n
      action_set[n] = v
    end
    append(static_data, n, ",")
  end
  append(static_data, "};\n")

  return table.concat(runtime {
    custom_data = table.concat(grammar.custom_data);
    action_data = table.concat(action_set);
    static_data = table.concat(static_data);
  })
end
