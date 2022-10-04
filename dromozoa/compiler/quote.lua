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
  ["\00"] = [[\u0000]]; ["\01"] = [[\u0001]]; ["\02"] = [[\u0002]]; ["\03"] = [[\u0003]];
  ["\04"] = [[\u0004]]; ["\05"] = [[\u0005]]; ["\06"] = [[\u0006]]; ["\07"] = [[\u0007]];
  ["\08"] = [[\b]];     ["\09"] = [[\t]];     ["\10"] = [[\n]];     ["\11"] = [[\v]];
  ["\12"] = [[\f]];     ["\13"] = [[\r]];     ["\14"] = [[\u000E]]; ["\15"] = [[\u000F]];
  ["\16"] = [[\u0010]]; ["\17"] = [[\u0011]]; ["\18"] = [[\u0012]]; ["\19"] = [[\u0013]];
  ["\20"] = [[\u0014]]; ["\21"] = [[\u0015]]; ["\22"] = [[\u0016]]; ["\23"] = [[\u0017]];
  ["\24"] = [[\u0018]]; ["\25"] = [[\u0019]]; ["\26"] = [[\u001A]]; ["\27"] = [[\u001B]];
  ["\28"] = [[\u001C]]; ["\29"] = [[\u001D]]; ["\30"] = [[\u001E]]; ["\31"] = [[\u001F]];
  ["\34"] = [[\"]];
  ["\92"] = [[\\]];
}

local LS = "\226\128\168" -- U+2028 | E2 80 A8 | LINE SEPARATOR
local PS = "\226\128\169" -- U+2029 | E2 80 A9 | PARAGRAPH SEPARATOR

return function (s)
  return '"' .. s:gsub("[%z\1-\31\"\\]", quote):gsub(LS, [[\u2028]]):gsub(PS, [[\u2029]]) .. '"'
end
