function a()
  print()
  print(a)
  print(a,b)
  print(a,b,c)
  return
end
function b(a)
  return a
end
function c(a,b)
  return a,b
end
function d(a,b,c)
  return a,b,c
end
function v(...) end
function av(a, ...) end
