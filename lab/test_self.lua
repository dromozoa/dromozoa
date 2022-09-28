local f = function (a, b, c, d)
  print(a, b, c, d)
end

local x = setmetatable({}, { __call = f })
local y = { x = x }

print("x", x)
print("y", y)
print "y:x(1,2,3,4)"
y:x(1,2,3,4)
print "x(y,1,2,3,4)"
x(y,1,2,3,4)
print "x(y,1,2,3,4)"
x(y,1,2,3,4)
print "f(x,y,1,2,3,4)"
f(x,y,1,2,3,4)
