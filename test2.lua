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
end

__export_start(main)
