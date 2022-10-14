-- Copyright (C) 2022 Tomoyuki Fujimori <moyu@dromozoa.com>
--
-- This file is part of dromozoa.
--
-- dromozoa is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- dromozoa is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with dromozoa.  If not, see <http://www.gnu.org/licenses/>.

-- indeterminate
-- static_function "require"

-- local static_env = {
--   require = static_function "require"
-- }

local lua54_parser = require "dromozoa.compiler.lua54_parser"

local indeterminate = {}

local function compiler_warning(message, u)
  if u and u.f and u.n and u.c then
    io.stderr:write(u.f..":"..u.n..":"..u.c..": compiler warning ("..message..")\n")
  else
    io.stderr:write("compiler warning ("..message..")\n")
  end
end

local function new(v)
  return { n = 1, v }
end

local function use(var)
  return var[var.n]
end

local function def(var, v)
  local n = var.n + 1
  var.n = n
  var[n] = v
end

local function get_table(t, k)
  local v = t[1][k]
  if v == nil and not t[2][k] then
    return indeterminate
  else
    return v
  end
end

local function new_table()
  return { {}, {} }
end

local function set_table(t, k, v)
  t[1][k] = v
  t[2][k] = true
end

local function static_require(modname)
  print("static_require", modname)
  return indeterminate
end

local env = new_table()
set_table(env, "require", static_require)

local function evaluate_code(modules, chunk, proto, state, u)
  local u_name = u[0]
  local a = u.a
  local b = u.b
  local t = u.top

  local S = state.stack
  local V = state.locals
  local U = state.upvalues

  if u_name == "push_nil" then
    for i = t + 1, t + a do
      S[i] = nil
    end

  elseif u_name == "push_false" then
    S[t + 1] = false

  elseif u_name == "push_true" then
    S[t + 1] = true

  elseif u_name == "push_literal" then
    S[t + 1] = a

  elseif u_name == "push_numeral" then
    S[t + 1] = assert(tonumber(a))

  elseif u_name == "new_table" then
    S[t + 1] = new_table()

  -- closure!!!

  elseif u_name == "pop" then
    -- noop

  elseif u_name == "get_local" then
    S[t + 1] = use(V[a])

  elseif u_name == "get_upvalue" then
    S[t + 1] = use(U[a])

  elseif u_name == "get_table" then
    local x = get_table(S[t - 1], S[t])
    if x == indeterminate then
      return "indeterminate"
    end
    S[t - 1] = x

  elseif u_name == "new_local" then
    V[a] = new(S[t])

  elseif u_name == "set_local" then
    def(V[a], S[t])

  elseif u_name == "set_upvalue" then
    def(U[a], S[t])

  elseif u_name == "set_table" then
    set_table(S[a], S[t - 1], S[t])

  elseif u_name == "set_field" then
    set_table(S[a], S[b], S[t])

  elseif u_name == "set_list" then
    -- TODO 厳密な実装が必要か？
    local x = S[a]
    for i = a + 1, t do
      set_table(x, i - a, S[i])
    end

  elseif u_name == "not" then
    S[t] = not S[t]

  elseif u_name == "if" then
    for _, v in ipairs(u[S[t] and 1 or 2]) do
      local message = evaluate_code(modules, chunk, proto, state, v)
      if message then
        return message
      end
    end

  elseif u_name == "return" then
    assert(not state.result)
    local R = { n = t }
    for i = 1, t do
      R[i] = S[i]
    end
    state.result = R

  else
    return "not supported"
  end
end

local function evaluate_proto(modules, chunk, proto)
  local state = {
    stack = {};
    locals = {};
    upvalues = { new(env) };
    labels = {};
  }
  for _, v in ipairs(proto.code) do
    local message = evaluate_code(modules, chunk, proto, state, v)
    if message then
      compiler_warning(message..": "..v[0], v.node)
      break
    end
  end
  if state.result then
    print("=>", table.unpack(state.result, 1, state.result.n))
  end
end

return function (chunks, chunk)
  evaluate_proto(chunks, chunk, chunk[1])
end
