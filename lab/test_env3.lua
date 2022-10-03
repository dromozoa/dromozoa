print("test_env3", _ENV, _ENV._ENV)
local env = {}
for k, v in pairs(_ENV) do
  env[k] = v
end
_ENV = env
print("test_env3", _ENV, _ENV._ENV)

