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

---@alias result
---|0 # ok
---|1 # err

---@param main fun(): result
function export_start(main)
  local code = main()
  os.exit(code)
end

---@return string[]
function get_arguments()
  local result = {}
  for i = 1, #arg do
    table.insert(result, arg[i])
  end
  return result
end

---@param filename string
---@return string
function read_file(filename)
  local handle <close> = assert(io.open(filename, "rb"))
  return handle:read "a"
end
