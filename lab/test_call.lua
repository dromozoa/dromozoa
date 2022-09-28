local f = function (a, b, c, d)
  print(a, b, c, d)
end

local x = setmetatable({}, { __call = f })
local y = setmetatable({}, { __call = x })
local z = setmetatable({}, { __call = z })

print(f, x, y, z)

print "z(1,2,3,4)"
z(1,2,3,4)

