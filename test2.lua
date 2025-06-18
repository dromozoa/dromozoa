local A = 42
local B = 17


function main(x, a)
  local a, b = main(17, a)

  for i = 1, 10 do
    a = 12
    local v = a * b * i
  end

  return A, b, b
end

-- export_start(main)
