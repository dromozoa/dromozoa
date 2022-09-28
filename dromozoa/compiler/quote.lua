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

local quote = {}
for byte = 0x00, 0x1F do
  quote[string.char(byte)] = ([[\x%02X]]):format(byte)
end
quote["\b"] = [[\b]]
quote["\t"] = [[\t]]
quote["\n"] = [[\n]]
quote["\v"] = [[\v]]
quote["\f"] = [[\f]]
quote["\r"] = [[\r]]
quote["\""] = [[\"]]
quote["\\"] = [[\\]]

local LS = string.char(0xE2, 0x80, 0xA8) -- U+2028 LINE SEPARATOR
local PS = string.char(0xE2, 0x80, 0xA9) -- U+2029 PARAGRAPH SEPARATOR

return function (s)
  return '"' .. s:gsub("[%z\1-\31\"\\]", quote):gsub(LS, [[\u2028]]):gsub(PS, [[\u2029]]) .. '"'
end
