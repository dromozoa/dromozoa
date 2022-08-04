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

local array = require "dromozoa.array"
local compare = require "dromozoa.compare"

local verbose = os.getenv "VERBOSE" == "1"

local function check_error(fn)
  local status, message = pcall(fn)
  if verbose then
    print(message)
  end
  assert(not status)
end

local x = array.fill(16, 42)
assert(x:size() == 16)
for _, v in x:ipairs() do
  assert(v == 42)
end
local y = x:slice()
assert(x ~= y)
assert(compare(x, y) == 0)

assert(compare(x:slice(18, 17), array()) == 0)
check_error(function() array(nil) end)
check_error(function() array(nil, 2) end)
check_error(function() array(1, nil) end)
check_error(function() array(1, nil, 3) end)
check_error(function() x:slice(0) end)
check_error(function() x:slice(1, 17) end)
check_error(function() x:slice(17, 17) end)
