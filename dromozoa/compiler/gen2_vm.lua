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

local append = require "dromozoa.append"
local compiler_error = require "dromozoa.compiler.compiler_error"
local lua54_regexp = require "dromozoa.compiler.lua54_regexp"
local lua54_parser = require "dromozoa.compiler.lua54_parser"
local generate = require "dromozoa.compiler.generate"
local parse = require "dromozoa.annotation.parse"
local table_unpack = table.unpack or unpack
local table_pack = table.pack or function (...)
  return { n = select("#", ...), ... }
end

local function new_var(context, decl, value)
  local variable = { decl = decl, value }
  variable.index = append(context.variables, variable)
  return variable
end

local function def_var(var, value)
  var[1] = value
end

local function use_var(var)
  return var[1]
end

local function push(stack, value)
  local n = stack.n + 1
  stack.n = n
  stack[n] = value
end

local function pop(stack)
  local n = stack.n
  stack.n = n - 1
  return stack[n]
end

local call

local function new_table(determinate)
  return { table = {}, determinate = determinate }
end

local function set_table(t, k, v)
  t.table[k] = v
  if t.determinate then
    t.determinate[k] = true
  end
end

local function get_metafield(context, t, ev)
  if type(t) == "string" then
    return context.string_metatable.table[ev]
  elseif t.metatable ~= nil then
    return t.metatable.table[ev]
  end
end

local function get_table(context, t, k, u)
  if type(t) ~= "string" then
    local v = t.table[k]
    if v ~= nil then
      return v
    end
  end
  local metafield = get_metafield(context, t, "__index")
  if metafield ~= nil then
    if metafield.table then
      local v = get_table(context, metafield, k, u)
      if v ~= nil then
        return v
      end
    else
      local v = call(context, metafield, t, k)[1]
      if v ~= nil then
        return v
      end
    end
  end
  if t.determinate and not t.determinate[k] then
    compiler_error("indeterminate", u.node)
  end
end

local function new_closure(context, chunk, proto, upvalues)
  local closure = { chunk = chunk, proto = proto, upvalues = upvalues }
  closure.index = append(context.closures, closure)
  return closure
end

local function process_closure(context, closure, ...)
  local P = closure.proto
  local U = closure.upvalues
  local S = { n = 0 }
  local V = {}
  for i = 1, P.nparams do
    V[i] = new_var(context, P.locals[i], select(i, ...))
  end
  local vararg = table_pack(select(P.nparams + 1, ...))

  local flat_code = P.flat_code
  local pc = 1

  while true do
    local u = flat_code[pc]
    if not u then
      break
    end
    pc = pc + 1

    local u_name = u[0]
    local a = u.a
    local b = u.b

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
      push(S, new_table())

    elseif u_name == "closure" then
      local chunk = closure.chunk
      local proto = chunk[a]
      local upvalues = {}
      for i, v in ipairs(proto.upvalues) do
        if v.var < 0 then
          upvalues[i] = U[-v.var]
        else
          upvalues[i] = V[v.var]
        end
      end
      push(S, new_closure(context, chunk, proto, upvalues))

    elseif u_name == "pop" then
      S.n = S.n - a

    elseif u_name == "get_local" then
      push(S, use_var(V[a]))

    elseif u_name == "get_upvalue" then
      push(S, use_var(U[a]))

    elseif u_name == "get_table" then
      local y = pop(S)
      local x = pop(S)
      push(S, get_table(context, x, y, u))

    elseif u_name == "new_local" then
      V[a] = new_var(context, P.locals[a], pop(S))

    elseif u_name == "set_local" then
      def_var(V[a], pop(S))

    elseif u_name == "set_upvalue" then
      def_var(U[a], pop(S))

    elseif u_name == "set_table" then
      local y = pop(S)
      local x = pop(S)
      set_table(S[a], x, y)
      if b then
        assert(a == S.n)
        pop(S)
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

    elseif u_name == "len" then
      local x = pop(S)
      if type(x) == "string" then
        push(S, #x)
      else
        push(S, #x.table)
      end

    elseif u_name == "if" then
      if not pop(S) then
        pc = P.labels[a].address
      end

    elseif u_name == "check_for" then
      def_var(V[a], assert(tonumber(use_var(V[a]))))
      def_var(V[a + 1], assert(tonumber(use_var(V[a + 1]))))
      if b == 3 then
        local step = assert(tonumber(use_var(V[a + 2])))
        assert(step ~= 0)
        def_var(V[a + 2], step)
      end

    elseif u_name == "loop" or u_name == "label" then
      -- ignore

    elseif u_name == "break" or u_name == "goto" or u_name == "exit"then
      pc = P.labels[a].address

    elseif u_name == "call" then
      local x = call(context, S[a], table_unpack(S, a + 1, S.n))
      S.n = a - 1
      for i = 1, b < 0 and x.n or b do
        push(S, x[i])
      end

    elseif u_name == "self" then
      local x = S[a]
      local y = call(context, get_table(context, x, S[a + 1], u), x, table_unpack(S, a + 2, S.n))
      S.n = a - 1
      for i = 1, b < 0 and y.n or b do
        push(S, y[i])
      end

    elseif u_name == "vararg" then
      if P.index == 1 then
        compiler_error("indeterminate", u.node)
      end

      for i = 1, a < 0 and vararg.n or a do
        push(S, vararg[i])
      end

    elseif u_name == "return" then
      break

    elseif u_name == "close" then
      local x = use_var(V[a])
      if x ~= nil then
        call(context, get_metafield(context, x, "__close"), x)
      end

    else
      compiler_error("not supported: "..u_name, u.node)
    end
  end

  return table_unpack(S, 1, S.n)
end

function call(context, f, ...)
  if type(f) == "function" then
    return table_pack(f(...))
  elseif f.table then
    return call(context, get_metafield(context, f, "__call"), f, ...)
  else
    return table_pack(process_closure(context, f, ...))
  end
end

local function process_chunk(context, chunk)
  append(context.chunks, chunk)
  local result = process_closure(context, new_closure(context, chunk, chunk[1], { new_var(context, chunk.env, context.env) }))
  if result == nil then
    return true
  else
    return result
  end
end

local function initialize(context, enable_print)
  local env = context.env

  set_table(env, "type", function (v)
    local t = type(v)
    if t ~= "table" then
      return t
    elseif v.table then
      return "table"
    else
      return "function"
    end
  end)

  set_table(env, "ipairs", function (t)
    return ipairs(t.table)
  end)

  set_table(env, "pairs", function (t)
    local metafield = get_metafield(context, t, "__pairs")
    if metafield ~= nil then
      local result = call(context, metafield, t)
      return result[1], result[2], result[3]
    end
    return pairs(t.table)
  end)

  set_table(env, "setmetatable", function (t, metatable)
    t.metatable = metatable
    return t
  end)

  set_table(env, "getmetatable", function (t)
    local metafield = get_metafield(context, t, "__metatable")
    if metafield ~= nil then
      return metafield
    end
    return t.metatable
  end)

  local package_loaded = {}
  set_table(env, "require", function (name)
    local module = package_loaded[name]
    if module ~= nil then
      return module
    end

    local filename = name:gsub("%.", "/")..".lua"
    local handle = assert(io.open(filename))
    local source = handle:read "*a"
    handle:close()
    local module = process_chunk(context, generate(lua54_regexp(source, filename, lua54_parser.max_terminal_symbol, lua54_parser())))
    package_loaded[name] = module
    return module
  end)

  set_table(env, "dromozoa_annotation_closure", function (annotation, f)
    f.annotation = parse(annotation)
    return f
  end)

  set_table(env, "dromozoa_annotation_main", function (f)
    f.main = true
    return f
  end)
  set_table(env, "dromozoa_annotation_main", nil)

  set_table(env, "dromozoa_annotation_export", function (export, f)
    f.export = export
    return f
  end)

  if enable_print then
    set_table(env, "print", print)
  end
end

return function (chunk, enable_print)
  local context = {
    env = new_table {};
    chunks = {};
    variables = {};
    closures = {};
  }
  initialize(context, enable_print)
  process_chunk(context, chunk)
  return context
end
