local g = "foo\nbar\x7Fあいうえお"
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

function main()
  local s = "Hello World"
  local data, size = unpack_string(s)
  -- allocate(16)
  -- fd_write(0, 0, 0, 0)
  -- local t = 3 * stack_pointer
  -- local u = g
end
