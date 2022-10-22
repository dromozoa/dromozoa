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

local compiler_error = require "dromozoa.compiler.compiler_error"
local lua54_regexp = require "dromozoa.compiler.lua54_regexp"
local lua54_parser = require "dromozoa.compiler.lua54_parser"
local generate = require "dromozoa.compiler.generate"
local parse = require "dromozoa.annotation.parse"
local table_unpack = table.unpack or unpack
local table_pack = table.pack or function (...)
  return { n = select("#", ...), ... }
end

local indeterminate = {}
local string_metatable

local function new_var(value)
  return { value }
end

local function def_var(var, value)
  var[1] = value
end

local function use_var(var)
  return var[1]
end

local function new_table(determinate)
  return { table = {}, determinate = determinate }
end

local function set_table(t, k, v)
  t.table[k] = v
  if t.determinate then
    t.determinate[k] = true
  end
end

local function get_metafield(t, ev)
  if type(t) == "string" then
    return string_metatable.table[ev]
  elseif t.metatable ~= nil then
    return t.metatable.table[ev]
  end
end

local call

local function get_table(t, k)
  if type(t) ~= "string" then
    local v = t.table[k]
    if v ~= nil then
      return v
    end
  end
  local metafield = get_metafield(t, "__index")
  if metafield ~= nil then
    if metafield.table then
      local v = get_table(metafield, k)
      if v ~= nil then
        return v
      end
    else
      local v = call(metafield, t, k)[1]
      if v ~= nil then
        return v
      end
    end
  end
  if t.determinate and not t.determinate[k] then
    error(indeterminate, 0)
  end
end

local function new_closure(chunk, proto, upvalues)
  return { chunk = chunk, proto = proto, upvalues = upvalues }
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

local function process_closure(chunk, proto, U, ...)
  local S = { n = 0 }
  local V = {}
  for i = 1, proto.nparams do
    V[i] = new_var(select(i, ...))
  end
  local vararg = table_pack(select(proto.nparams + 1, ...))

  local flat_code = proto.flat_code
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

    -- print("["..(pc - 1).."]", u.node.f, u.node.n, u.node.c, u_name)

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
      local upvalues = {}
      for i, v in ipairs(chunk[a].upvalues) do
        if v.var < 0 then
          upvalues[i] = U[-v.var]
        else
          upvalues[i] = V[v.var]
        end
      end
      push(S, new_closure(chunk, chunk[a], upvalues))

    elseif u_name == "pop" then
      S.n = S.n - a

    elseif u_name == "get_local" then
      push(S, use_var(V[a]))

    elseif u_name == "get_upvalue" then
      push(S, use_var(U[a]))

    elseif u_name == "get_table" then
      local y = pop(S)
      local x = pop(S)
      push(S, get_table(x, y))

    elseif u_name == "new_local" then
      V[a] = new_var(pop(S))

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
        pc = proto.labels[a].address
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
      pc = proto.labels[a].address

    elseif u_name == "call" then
      local x = call(S[a], table_unpack(S, a + 1, S.n))
      S.n = a - 1
      for i = 1, b < 0 and x.n or b do
        push(S, x[i])
      end

    elseif u_name == "self" then
      local x = S[a]
      local y = call(get_table(x, S[a + 1]), x, table_unpack(S, a + 2, S.n))
      S.n = a - 1
      for i = 1, b < 0 and y.n or b do
        push(S, y[i])
      end

    elseif u_name == "vararg" then
      if proto.index == 1 then
        error(indeterminate, 0)
      end

      for i = 1, a < 0 and vararg.n or a do
        push(S, vararg[i])
      end

    elseif u_name == "return" then
      break

    elseif u_name == "close" then
      local x = use_var(V[a])
      if x ~= nil then
        call(get_metafield(x, "__close"), x)
      end

    else
      compiler_error("not supported: "..u_name, u.node)
    end
  end

  return table_unpack(S, 1, S.n)
end

function call(f, ...)
  if type(f) == "function" then
    return table_pack(f(...))
  elseif f.table then
    return call(get_metafield(f, "__call"), f, ...)
  else
    return table_pack(process_closure(f.chunk, f.proto, f.upvalues, ...))
  end
end

local function process_chunk(chunk, env)
  return process_closure(chunk, chunk[1], { new_var(env) })
end

local function initialize_env(enable_print)
  local env = new_table {}

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
    local metafield = get_metafield(t, "__pairs")
    if metafield ~= nil then
      local result = call(metafield, t)
      return result[1], result[2], result[3]
    end
    return pairs(t.table)
  end)

  set_table(env, "setmetatable", function (t, metatable)
    t.metatable = metatable
    return t
  end)

  set_table(env, "getmetatable", function (t)
    local metafield = get_metafield(t, "__metatable")
    if metafield ~= nil then
      return metafield
    end
    return t.metatable
  end)

  local loaded = {}

  set_table(env, "require", function (name)
    local module = loaded[name]
    if module == nil then
      local filename = name:gsub("%.", "/")..".lua"
      local handle = assert(io.open(filename))
      local source = handle:read "*a"
      handle:close()
      local chunk = generate(lua54_regexp(source, filename, lua54_parser.max_terminal_symbol, lua54_parser()))
      module = process_chunk(chunk, env)
      if module == nil then
        module = true
      end
      loaded[name] = module
    end
    return module
  end)

  if enable_print then
    set_table(env, "print", print)
  end

  return env
end

local function initialize_string(env)
  local module = new_table {}

  set_table(module, "gsub", function (s, pattern, repl, n)
    if repl == "string" then
      return string.gsub(s, pattern, repl, n)
    elseif repl.table then
      return string.gsub(s, pattern, function (k) return get_table(repl, k) end, n)
    else
      return string.gsub(s, pattern, function (...) return table_unpack(call(repl, ...)) end, n)
    end
  end)

  set_table(env, "string", module)

  string_metatable = new_table {}
  set_table(string_metatable, "__index", module)
end

local function initialize_annotation(env)
  set_table(env, "dromozoa_annotation_closure", function (annotation, f)
    f.annotation = parse(annotation)
    return f
  end)

  set_table(env, "dromozoa_annotation_main", function (f)
    f.main = true
    return f
  end)

  set_table(env, "dromozoa_annotation_export", function (export, f)
    f.export = export
    return f
  end)
end

return function (chunk, enable_print)
  local env = initialize_env(enable_print)
  initialize_string(env)
  initialize_annotation(env)
  process_chunk(chunk, env)
end
