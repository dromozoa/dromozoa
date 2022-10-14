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

local table_pack = table.pack or function (...)
  return { n = select("#", ...), ... }
end

-- 決定論的に評価可能なコードを逐次実行する。
-- 1. 浮動小数点数の計算も対象とする。
-- 2. 数値forのラップアラウンドの挙動が合致しない。
-- 3. OP_SETLIST後の長さがエッジケースで合致しない。

local break_message = {}

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

local function new_table(map, determinate)
  local t = {}
  map[t] = { determinate = determinate }
  return t
end

local function get_table(map, t, k, u)
  local v = t[k]
  if v == nil then
    local determinate = map[t].determinate
    if determinate and not determinate[k] then
      evaluation_error("indeterminate: "..u[0], u.node)
    end
  end
  return v
end

local function set_table(map, t, k, v)
  t[k] = v
  local determinate = map[t].determinate
  if determinate then
    determinate = true
  end
end

local function set_table(t, k, v)
  t[k] = v
  if t.determinate then
    t.determinate[k] = true
  end
end

local function push(stack, v)
  local n = stack.n + 1
  stack.n = n
  stack[n] = v
end

local function pop(stack, n)
  local n = stack.n
  stack.n = n - 1
  return stack[n]
end

local evaluate_closure

local function evaluate_code(map, chunk, proto, state, u)
  local u_name = u[0]
  local a = u.a
  local b = u.b

  local S = state.stack
  local V = state.locals
  local U = state.upvalues

  if u_name == "push_nil" then
    for i = 1, a do
      push(S, nil)
    end

  elseif u_name == "push_false" then
    push(S, false)

  elseif u_name == "push_true" then
    push(S, true)

  elseif u_name == "push_literal" then
    push(S, a)

  elseif u_name == "push_numeral" then
    push(S, assert(tonumber(a)))

  elseif u_name == "new_table" then
    push(S, new_table(map))

  elseif u_name == "closure" then
    local closure_proto = chunk[a]
    local closure_upvalues = {}
    for i, v in ipairs(closure_proto.upvalues) do
      if v.var < 0 then
        upvalues[i] = U[-v.var]
      else
        upvalues[i] = V[v.var]
      end
    end
    local closure = function (...)
      return evaluate_closure(map, chunk, closure_proto, closure_upvalues, ...)
    end
    map[closure] = {
      chunk = chunk;
      proto = closure_proto;
      upvalues = closure_upvalues;
      parent_proto = proto;
      parent_state = state;
    }
    push(S, closure)

  elseif u_name == "pop" then
    for i = 1, a do
      pop(S)
    end

  elseif u_name == "get_local" then
    push(S, use_var(V[a]))

  elseif u_name == "get_upvalue" then
    push(S, use_var(U[a]))

  elseif u_name == "get_table" then
    local y = pop(S)
    local x = pop(S)
    push(S, get_table(x, y, u))

  elseif u_name == "new_local" then
    V[a] = new_var(pop(S))

  elseif u_name == "set_local" then
    def_var(V[a], pop(S))

  elseif u_name == "set_upvalue" then
    def_var(U[a], pop(S))

  elseif u_name == "set_table" then
    local y = pop(S)
    local x = pop(S)
    if b then
      assert(a == S.n)
      set_table(pop(S), x, y)
    else
      set_table(S[a], x, y)
    end

  elseif u_name == "set_field" then
    set_table(S[a], S[b], pop(S))

  elseif u_name == "set_list" then
    local x = S[a]
    for i = a + 1, S.n do
      set_table(x, i - a, S[i])
    end
    S.n = a

  elseif u_name == "add"    then local y, x = pop(S), pop(S) push(S, x + y)
  elseif u_name == "sub"    then local y, x = pop(S), pop(S) push(S, x - y)
  elseif u_name == "mul"    then local y, x = pop(S), pop(S) push(S, x * y)
  elseif u_name == "div"    then local y, x = pop(S), pop(S) push(S, x / y)
  elseif u_name == "idiv"   then local y, x = pop(S), pop(S) push(S, math.floor(x / y))
  elseif u_name == "mod"    then local y, x = pop(S), pop(S) push(S, x % y)
  elseif u_name == "pow"    then local y, x = pop(S), pop(S) push(S, x ^ y)
  elseif u_name == "concat" then local y, x = pop(S), pop(S) push(S, x .. y)
  elseif u_name == "lt"     then local y, x = pop(S), pop(S) push(S, x < y)
  elseif u_name == "le"     then local y, x = pop(S), pop(S) push(S, x <= y)
  elseif u_name == "gt"     then local y, x = pop(S), pop(S) push(S, x > y)
  elseif u_name == "ge"     then local y, x = pop(S), pop(S) push(S, x >= y)
  elseif u_name == "eq"     then local y, x = pop(S), pop(S) push(S, x == y)
  elseif u_name == "ne"     then local y, x = pop(S), pop(S) push(S, x ~= y)

  elseif u_name == "unm" then push(S, -pop(S))
  elseif u_name == "not" then push(S, not pop(S))
  elseif u_name == "len" then push(S, #pop(S))

  elseif u_name == "if" then
    for _, v in ipairs(u[pop(S) and 1 or 2]) do
      evaluate_code(map, chunk, proto, state, v)
    end

  elseif u_name == "check_for" then
    def_var(V[a], assert(tonumber(use_var(V[a]))))
    def_var(V[a + 1], assert(tonumber(use_var(V[a + 1]))))
    if b == 3 then
      local step = assert(tonumber(use_var(V[a + 2])))
      assert(step ~= 0)
      def_var(V[a + 2], step)
    end

  elseif u_name == "loop" then
    local _, message = pcall(function ()
      while true do
        for _, v in ipairs(u) do
          evaluate_code(map, chunk, proto, state, v)
        end
      end
    end)
    if message ~= break_message then
      error(message, 0)
    end

  elseif u_name == "label" then

  elseif u_name == "break" then
    error(break_message, 0)

  elseif u_name == "call" or u_name == "self" then
    local x = table_pack(S[a](table_unpack(S, a + 1, S.n)))
    S.n = a - 1
    for i = 1, b < 0 and x.n or b do
      push(S, x[i])
    end

  elseif u_name == "vararg" then
    if proto.index == 1 then
      evaluation_error("indeterminate: "..u_name, u.node)
    end

    for i = 1, a < 0 and state.vararg.n or a do
      push(S, state.vararg[i])
    end

  elseif u_name == "return" then
    local R = { n = 0 }
    for i = 1, S.n do
      push(R, S[i])
    end
    S.n = 0
    state.result = R

  else
    evaluation_error("not supported: "..u_name, u.node)
  end
end

function evaluate_closure(map, chunk, proto, upvalues, ...)
  local locals = {}
  for i = 1, proto.nparams do
    locals[i] = new_var(select(i, ...))
  end
  local labels = {}
  if #proto.labels > 0 then
    
  end

  local state = {
    stack = { n = 0 };
    vararg = table_pack(select(proto.nparams + 1, ...));
    locals = locals;
    upvalues = upvalues;
    labels = {};
    result = { n = 0 }
  }

  for _, v in ipairs(proto.code) do
    evaluate_code(map, chunk, proto, state, v)
  end
  return table_unpack(state.result, 1, state.result.n)
end

return function (chunk)
  local map = {}
  local env = new_table(map, {})
  set_table(env, "print", print)
  print("=>", evaluate_closure(map, chunk, chunk[1], { new_var(env) }))
end
