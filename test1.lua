function make_alignment(n, a)
  local r = n % a
  if r ~= 0 then
    n = n + a - r
  end
  return n
end

function allocate(n)
  local p = stack_pointer
  stack_pointer = p + make_alignment(n, 8)
  return p
end

function __new_table(n)
  local t = allocate(8)
  i32_store(t, allocate(n * 4))
  i32_store(t + 4, n)
  return t
end

function __set_table(t, i, v)
  i32_store(t + (i - 1) * 4, v)
  return t
end

function get(t, i)
  return i32_load(t + (i - 1) * 4)
end

local g = "foo\n"
local x = 42

function unpack_string(s)
  local data = i32_load(s)
  local size = i32_load(s + 4)
  return data, size
end

function write_string(s)
  local data, size = unpack_string(s)
  local item = allocate(8)
  i32_store(item, data)
  i32_store(item + 4, size)
  local out = allocate(4)
  i32_store(out, 0)
  fd_write(1, item, 1, out)
end

function write_i32(v)
  if v < 0 then
    write_string("-")
    write_i32(-v)
    return
  end

  local buffer = allocate(12)
  local p = buffer + 11
  i32_store8(p, 0)
  p = p - 1

  local n = 0
  while true do
    local r = v % 10
    v = v / 10

    i32_store8(p, r + 48)
    n = n + 1

    if v == 0 then
      break
    end
    p = p - 1
  end

  local s = allocate(8)
  i32_store(s, p)
  i32_store(s + 4, n)
  write_string(s)
end

function test()
  local ifelse = write_string
  write_i32(ifelse)
  write_string("\n")
  call_indirect0("test\n", ifelse)

  local f = unpack_string
  local data, size = call_indirect2(g, f)
  write_i32(data)
  write_string(",")
  write_i32(size)
  write_string("\n")
end

function test2(v)
  if v == 0 then
    write_string("foo\n")
  elseif v == 1 then
    write_string("bar\n")
  elseif v == 2 then
    write_string("baz\n")
  else
    write_string("qux\n")
  end
end

function test3()
  local t = { 1, 2 * 3, "foo\n" }
  write_i32(get(t, 1))
  write_string("\n")
  write_i32(get(t, 2))
  write_string("\n")
  write_string(get(t, 3))
end

function main()
  local s = "Hello World\n"
  local i = 0
  while i ~= 10 do
    write_string(s)
    i = i + 1
  end
  write_i32(42)
  write_string("\n")
  write_i32(-1234)
  write_string("\n")
  test()
  test2(0)
  test2(1)
  test2(2)
  test2(3)

  write_i32(1 <= 1)
  write_i32(1 <= 2)
  write_i32(1 < 1)
  write_i32(1 < 2)
  write_i32(1 >= 1)
  write_i32(2 >= 1)
  write_i32(1 > 1)
  write_i32(2 > 1)
  write_string("\n")

  test3()

  -- allocate(16)
  -- fd_write(0, 0, 0, 0)
  -- local t = 3 * stack_pointer
  -- local u = g
end
