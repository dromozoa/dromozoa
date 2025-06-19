local A = 42
local B = true
local C = "foo"

function test(a, b, c)
end

function main()
  local x = A
  local f = test
  __call_indirect0(f, 1, 2, 3)
end

__export_start(main)
