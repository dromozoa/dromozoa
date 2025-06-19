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

  test1()
  test2()
  test3()
  test4()
end

function test1()
  local t = { 17, 23, 42, 69 }
  io_write_integer(t[3])
  io_write_string("\n")
  local size, capacity, data = __unpack_table(t)
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

  io_write_integer(string_compare("bar", "barbar"))
  io_write_string("\n")

  io_write_string(string_sub(C, 4, 6).."\n")
  io_write_string(string_sub("foo", 3, 4).."\n")
end

function test4()
  local s = io_read_all()
  io_write_string(s)
  io_write_string("\n")
end

__export_start(main)
