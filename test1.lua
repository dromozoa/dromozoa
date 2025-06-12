local g = "foo\n"
local x = 42

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

function main()
  local s = "Hello World\n"
  local i = 0
  while i ~= 10 do
    write_string(s)
    i = i + 1
  end
  -- allocate(16)
  -- fd_write(0, 0, 0, 0)
  -- local t = 3 * stack_pointer
  -- local u = g
end
