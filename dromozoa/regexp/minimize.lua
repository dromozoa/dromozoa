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

return function (u)
  -- 基本はHopcroftのアルゴリズム
  -- Mooreのアルゴリズムでもよいかも？

  -- accept statesはacceptごとに分割する
  -- acceptが全部いっしょだったら完全な最小化
  -- 途中の状態はおなじ遷移とみなしたくなったらどうするの？

  while true do
    local partition = pop_partition()
    for byte = 0x00, 0xFF do
      -- byteでpartitionのなかの状態に遷移する遷移元の集合を計算する
    end




  end
end
