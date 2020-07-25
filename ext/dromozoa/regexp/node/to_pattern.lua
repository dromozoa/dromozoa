-- Copyright (C) 2020 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local encode_set = require "dromozoa.regexp.encode_set"

local prec_table = {
  ["["] = 1;
  ["*"] = 2; ["?"] = 2;
  ["."] = 3;
  ["|"] = 4;
}
local prec_start = 5

local function node_to_pattern(self, parent_prec)
  local code = self[0]
  if code == "[" then
    return encode_set(self[1])
  else
    local prec = prec_table[code]
    local group = prec > parent_prec

    local buffer = {}
    local n = 0

    if group then
      n = n + 1; buffer[n] = "("
    end

    n = n + 1; buffer[n] = node_to_pattern(self[1], prec)

    if code ~= "." then
      n = n + 1; buffer[n] = code
    end

    local b = self[2]
    if b then
      n = n + 1; buffer[n] = node_to_pattern(b, prec)
    end

    if group then
      n = n + 1; buffer[n] = ")"
    end

    return table.concat(buffer)
  end
end

return function (self)
  return node_to_pattern(self, prec_start)
end
