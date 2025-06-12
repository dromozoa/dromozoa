local g = "foo\nbar\x7Fあいうえお"
local x = 42

function make_alignment(n, a)
  local r = n % a == 0
end

function allocate_stack(n)
  local x = n * n
  return x
end

function main()
  local s, d = "Hello World", 42
  allocate_stack(17)
  -- fd_write(0, 0, 0, 0)
  -- local t = 3 * stack_pointer
  -- local u = g
end
