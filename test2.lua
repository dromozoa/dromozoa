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

  test()
end

function test()
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

__export_start(main)
