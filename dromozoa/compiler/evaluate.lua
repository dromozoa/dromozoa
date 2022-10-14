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

-- static_function "require"

-- local static_env = {
--   require = static_function "require"
-- }

local lua54_parser = require "dromozoa.compiler.lua54_parser"

local table_unpack = table.unpack or unpack

-- 決定論的に評価可能なコードを逐次実行する。
-- 1. 浮動小数点数の計算も対象とする。
-- 2. 数値forのラップアラウンドの挙動が合致しない。
-- 3. OP_SETLIST後の長さがエッジケースで合致しない。

local function evaluation_error(message, u)
  if u and u.f and u.n and u.c then
    error(u.f..":"..u.n..":"..u.c..": evaluation error ("..message..")\n")
  else
    error("evaluation error ("..message..")\n")
  end
end

local function new_var(v)
  return { n = 1, v }
end

local function use_var(var)
  return var[var.n]
end

local function def_var(var, v)
  local n = var.n + 1
  var.n = n
  var[n] = v
end

local function new_table()
  return { {} }
end

local function get_table(t, k, u)
  local v = t[1][k]
  if v == nil and t.determinate and not t.determinate[k] then
    evaluation_error("indeterminate: "..u[0], u.node)
  end
  return v
end

local function set_table(t, k, v)
  t[1][k] = v
  if t.determinate then
    t.determinate[k] = true
  end
end

local function get(v)
  if type(v) == "table" then
    return v[1]
  else
    return v
  end
end

local env = { {}, determinate = {} }
set_table(env, "print", print)

local evaluate_closure

local function evaluate_code(chunk, proto, state, u)
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
    S[t + 1] = tonumber(a)

  elseif u_name == "new_table" then
    S[t + 1] = new_table()

  elseif u_name == "closure" then
    local upvalues = {}
    for i, v in ipairs(chunk[a].upvalues) do
      if v.var < 0 then
        upvalues[i] = U[-v.var]
      else
        upvalues[i] = V[v.var]
      end
    end
    S[t + 1] = {
      function (...)
        return evaluate_closure(chunk, chunk[a], upvalues, ...)
      end;
      chunk = chunk;
      proto = chunk[a];
      upvalues = upvalues;
      parent_proto = proto;
      parent_state = state;
    }

  elseif u_name == "pop" then
    -- noop

  elseif u_name == "get_local" then
    S[t + 1] = use_var(V[a])

  elseif u_name == "get_upvalue" then
    S[t + 1] = use_var(U[a])

  elseif u_name == "get_table" then
    S[t - 1] = get_table(S[t - 1], S[t], u)

  elseif u_name == "new_local" then
    V[a] = new_var(S[t])

  elseif u_name == "set_local" then
    def_var(V[a], S[t])

  elseif u_name == "set_upvalue" then
    def_var(U[a], S[t])

  elseif u_name == "set_table" then
    set_table(S[a], S[t - 1], S[t])

  elseif u_name == "set_field" then
    set_table(S[a], S[b], S[t])

  elseif u_name == "set_list" then
    local x = S[a]
    for i = a + 1, t do
      set_table(x, i - a, S[i])
    end

  elseif u_name == "not" then
    S[t] = not S[t]

  elseif u_name == "if" then
    for _, v in ipairs(u[S[t] and 1 or 2]) do
      local message = evaluate_code(chunk, proto, state, v)
      if message then
        return message
      end
    end

  elseif u_name == "call" then
    local x = table.pack(get(S[a])(table_unpack(S, a + 1, t < 0 and S.n or t)))
    for i = 1, b < 0 and x.n or b do
      S[a + i - 1] = x[i]
    end
    S.n = a + (b < 0 and x.n or b) - 1
    for i = a, S.n do
      S[i] = x[i - a + 1]
    end

  elseif u_name == "vararg" then
    if proto.index == 1 then
      evaluation_error("not supported: "..u_name, u.node)
    end

    S.n = t + (a < 0 and state.vararg.n or a)
    for i = t + 1, S.n do
      S[i] = state.vararg[i - t]
    end


  elseif u_name == "return" then
    local R = { n = t < 0 and S.n or t }
    for i = 1, R.n do
      R[i] = S[i]
    end
    state.result = R

  else
    evaluation_error("not supported: "..u_name, u.node)
  end
end

function evaluate_closure(chunk, proto, upvalues, ...)
  local locals = {}
  for i = 1, proto.nparams do
    locals[i] = new_var(select(i, ...))
  end
  local state = {
    stack = {};
    vararg = table.pack(select(proto.nparams + 1, ...));
    locals = locals;
    upvalues = upvalues;
    labels = {};
    result = { n = 0 }
  }

  for _, v in ipairs(proto.code) do
    evaluate_code(chunk, proto, state, v)
  end
  return table_unpack(state.result, 1, state.result.n)
end

return function (chunk)
  print("=>", evaluate_closure(chunk, chunk[1], { new_var(env) }))
end
