-- Copyright (C) 2025 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local A = 42
local B = true
local C = "foobarbaz\n"

function main()
  local A = A * 2
  io_write_string(C)
  io_write_integer(A)
  io_write_string("\n")

  local i = 0
  while i < 4 do
    i = i + 1
    io_write_integer(i)
  end
  io_write_string("\n")

  for i = 1, 4 do
    io_write_integer(i)
  end
  io_write_string("\n")

  for i = 6, 1, -2 do
    io_write_integer(i)
  end
  io_write_string("\n")

  io_write_integer(#C)
  io_write_string("\n")

  local tests = { test1, test2, test3, test4, test5, test6, test7 }
  for i = 1, #tests do
    __call_indirect0(tests[i])
  end
end

function test1()
  local t = { 17, 23, 42, 69 }
  io_write_integer(t[3])
  io_write_string("\n")
  t[5], t[4] = -1, 0x69

  for i = 1, #t do
    io_write_integer(t[i])
    io_write_string("\n")
  end
end

function test2()
  local a = "foo"
  local b = "bar"
  local c = "baz"

  io_write_string(a..integer_to_string(42)..b..c.."\n")
  -- error("ERROR!")
end

function test3()
  local t = { 0x30, 0x41, 0x61 }
  table_insert(t, 0x7A)
  local s = string_char(t)
  io_write_string(s.."\n")

  if string_compare("bar", "barbar") >= 0 then
    error('string_compare("bar", "barbar") >= 0')
  end

  io_write_string(string_sub(C, 4, 6).."\n")
  io_write_string(string_sub("foo", 3, 4).."\n")
end

function test4()
  local s = io_read_all()
  io_write_string(s)
  io_write_string("\n")
end

function f_true()
  io_write_string("f_true\n")
  return true
end

function f_false()
  io_write_string("f_false\n")
  return false
end

function test5()
  local cond = true

  if not not cond then
    io_write_string("x\n")
  else
    io_write_string("y\n")
  end

  io_write_string("--\n")
  local _ = f_true() or f_false()
  io_write_string("--\n")
  local _ = f_true() and f_false() or f_true()
  io_write_string("--\n")
end

function test6()
  if 3^3 ~= 27 then
    error("3^3 ~= 27")
  end

  io_write_integer(0x7FFFFFFF & ~0xDEAD)
  io_write_string("\n")
  io_write_integer(0x1234 ~ 0xFEDC)
  io_write_string("\n")
end

function write(s)
  io_write_string(s)
  return 0
end

function test7()
  -- io_write_string "stat\n"
  -- local r = write "exp\n"
end

__export_start(main)
