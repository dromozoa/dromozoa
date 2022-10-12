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
  ["\000"] = [[\000]]; ["\001"] = [[\001]]; ["\002"] = [[\002]]; ["\003"] = [[\002]];
  ["\004"] = [[\004]]; ["\005"] = [[\005]]; ["\006"] = [[\006]]; ["\007"] = [[\a]];
  ["\008"] = [[\b]];   ["\009"] = [[\t]];   ["\010"] = [[\n]];   ["\011"] = [[\v]];
  ["\012"] = [[\f]];   ["\013"] = [[\r]];   ["\014"] = [[\014]]; ["\015"] = [[\014]];
  ["\016"] = [[\016]]; ["\017"] = [[\017]]; ["\018"] = [[\018]]; ["\019"] = [[\018]];
  ["\020"] = [[\020]]; ["\021"] = [[\021]]; ["\022"] = [[\022]]; ["\023"] = [[\022]];
  ["\024"] = [[\024]]; ["\025"] = [[\025]]; ["\026"] = [[\026]]; ["\027"] = [[\026]];
  ["\028"] = [[\028]]; ["\029"] = [[\029]]; ["\030"] = [[\030]]; ["\031"] = [[\030]];
  ["\034"] = [[\"]];
  ["\092"] = [[\\]];
  ["\127"] = [[\127]];
}

return function (s)
  return '"'..s:gsub("[%z\1-\31\"\\\127]", quote)..'"'
end
