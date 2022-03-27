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

local fsm = require "dromozoa.regexp.fsm"
local minimize = require "dromozoa.regexp.minimize"
local nfa_to_dfa = require "dromozoa.regexp.nfa_to_dfa"
local tree_to_nfa = require "dromozoa.regexp.tree_to_nfa"

return function (token_names, data)
  local definitions = {}

  for k, v in pairs(data) do
    local name
    if type(k) == "string" then
      name = k
    else
      name = v.literal
    end
    definitions[#definitions + 1] = { timestamp = assert(v.timestamp), name = name, def = v }
  end

  table.sort(definitions, function (a, b) return a.timestamp < b.timestamp end)

  local u = fsm.new_state()
  local timestamp

  for i = 1, #definitions do
    local v, w = tree_to_nfa(definitions[i].def)
    fsm.new_transition(u, v)
    local t = v.timestamp
    if not timestamp or timestamp > t then
      timestamp = t
    end

    local name = definitions[i].name
    if name then
      local symbol = #token_names + 1
      token_names[symbol] = name

      local accept_action = w.accept_action
      if accept_action == true then
        w.accept_action = "token_symbol=" .. symbol .. "; push_token()"
      else
        w.accept_action = "token_symbol=" .. symbol .. "; " .. accept_action
      end
    end
  end
  u.timestamp = timestamp

  local u = minimize(nfa_to_dfa(u))
  u.loop = true
  return u
end
