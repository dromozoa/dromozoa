-- Copyright (C) 2026 Tomoyuki Fujimori <moyu@dromozoa.com>
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
-- along with dromozoa.  If not, see <https://www.gnu.org/licenses/>.

local numbers = {
  3,
  345,
  0xff,
  0xBEBADA,
  3.0,
  3.1416,
  314.16e-2,
  0.31416E1,
  34e1,
  0x0.1E,
  0xA23p-4,
  0X1.921FB54442D18P+1,

  0x.CD,
  0xAB.CD,
  0xAB.,
  0x.CDp1,
  0xAB.CDp1,
  0xAB.p1,
  0x.CDp-1,
  0xAB.CDp-1,
  0xAB.p-1,
  0xABp1,
  0xABp-1,

  .34,
  12.34,
  12.,
  .34e1,
  12.34e1,
  12.e1,
  .34e-1,
  12.34e-1,
  12.e-1,
  12e1,
  12e-1,

  0xABCD,
  1234,
}

for _, v in ipairs(numbers) do
  if math.type(v) == "integer" then
    io.write(("integer: %d\n"):format(v))
  else
    io.write(("float: %A\n"):format(v))
  end
end
