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

return function (token_names, that)
  local data = {}
  for name, item in pairs(that) do
    if type(name) ~= "string" then
      name = item.literal
    end
    data[#data + 1] = { timestamp = item.timestamp, name = name, item = item }
  end
  table.sort(data, function (a, b) return a.timestamp < b.timestamp end)

  local u = fsm.new_state()
  local timestamp
  for i = 1, #data do
    local v, w = tree_to_nfa(data[i].item)
    fsm.new_transition(u, v)
    timestamp = fsm.merge_timestamp(timestamp, v.timestamp)
    local name = data[i].name
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
