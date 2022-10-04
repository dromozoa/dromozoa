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

local quote = {
  ["\000"] = [[\u0000]]; ["\001"] = [[\u0001]]; ["\002"] = [[\u0002]]; ["\003"] = [[\u0003]];
  ["\004"] = [[\u0004]]; ["\005"] = [[\u0005]]; ["\006"] = [[\u0006]]; ["\007"] = [[\u0007]];
  ["\008"] = [[\b]];     ["\009"] = [[\t]];     ["\010"] = [[\n]];     ["\011"] = [[\v]];
  ["\012"] = [[\f]];     ["\013"] = [[\r]];     ["\014"] = [[\u000E]]; ["\015"] = [[\u000F]];
  ["\016"] = [[\u0010]]; ["\017"] = [[\u0011]]; ["\018"] = [[\u0012]]; ["\019"] = [[\u0013]];
  ["\020"] = [[\u0014]]; ["\021"] = [[\u0015]]; ["\022"] = [[\u0016]]; ["\023"] = [[\u0017]];
  ["\024"] = [[\u0018]]; ["\025"] = [[\u0019]]; ["\026"] = [[\u001A]]; ["\027"] = [[\u001B]];
  ["\028"] = [[\u001C]]; ["\029"] = [[\u001D]]; ["\030"] = [[\u001E]]; ["\031"] = [[\u001F]];
  ["\034"] = [[\"]];
  ["\092"] = [[\\]];
  ["\127"] = [[\u007F]];
}

local LS = "\226\128\168" -- U+2028 | E2 80 A8 | LINE SEPARATOR
local PS = "\226\128\169" -- U+2029 | E2 80 A9 | PARAGRAPH SEPARATOR

return function (s)
  return '"' .. s:gsub("[%z\1-\31\"\\\127]", quote):gsub(LS, [[\u2028]]):gsub(PS, [[\u2029]]) .. '"'
end
