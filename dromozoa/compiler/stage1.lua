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
local quote_js = require "dromozoa.quote_js"
local compiler_error = require "dromozoa.compiler.compiler_error"
local stage1_prologue = require "dromozoa.compiler.stage1_prologue"

local function push_stack(map, i)
  local stack = map.stack

  assert(not stack[i])
  local n = map.n + 1
  map.n = n
  map[n] = { use = 0 }

  stack[i] = n
  return n
end

local function get_stack(map, i)
  local stack = map.stack

  local n = assert(stack[i])
  local t = assert(map[n])
  t.use = t.use + 1

  return n
end

local function pop_stack(map, i, store)
  local stack = map.stack

  local n = assert(stack[i])
  local t = assert(map[n])
  t.use = t.use + 1
  t.store = store

  stack[i] = nil
  return n
end

local function push_stack_range(map, m, n)
  local result = {}
  for i = m, n do
    append(result, push_stack(map, i))
  end
  return result
end

local function pop_stack_range(map, m, n)
  if n < 0 then
    n = -n - 1
  end

  local result = {}
  for i = m, n do
    append(result, pop_stack(map, i))
  end
  return result
end

local opcodes = {
  add    = "binop";
  sub    = "binop";
  mul    = "binop";
  div    = "binop";
  idiv   = "binop";
  pow    = "binop";
  band   = "binop";
  bxor   = "binop";
  bor    = "binop";
  shr    = "binop";
  shl    = "binop";
  concat = "binop";
  lt     = "binop";
  le     = "binop";
  gt     = "binop";
  ge     = "binop";
  eq     = "binop";
  ne     = "binop";

  unm  = "unop";
  len  = "unop";
  bnot = "unop";

  new_local   = "pop";
  tbc_local   = "pop";

  get_local    = "push";
  get_upvalue  = "push";
  new_table    = "push";
  closure      = "push";
  push_false   = "push";
  push_true    = "push";
  push_literal = "push";
  push_numeral = "push";
}

local function process_code(map, u)
  local u_name = u[0]
  local a = u.a
  local b = u.b
  local t = u.top

  local opcode = opcodes[u_name]
  if opcode == "binop" then
    u.x = pop_stack(map, t - 1)
    u.y = pop_stack(map, t)
    u.z = push_stack(map, t - 1)
  elseif opcode == "unop" then
    u.x = pop_stack(map, t)
    u.y = push_stack(map, t)
  elseif opcode == "pop" then
    u.x = pop_stack(map, t)
  elseif opcode == "push" then
    u.x = push_stack(map, t + 1)

  elseif u_name == "if" then
    u.x = pop_stack(map, t, true)
    for _, v in ipairs(u[1]) do
      process_code(map, v)
    end
    for _, v in ipairs(u[2]) do
      process_code(map, v)
    end
  elseif u_name == "loop" then
    for _, v in ipairs(u) do
      process_code(map, v)
    end
  elseif u_name == "mod" then
    u.x = pop_stack(map, t - 1)
    u.y = pop_stack(map, t, true)
    u.z = push_stack(map, t - 1)
  elseif u_name == "not" then
    u.x = pop_stack(map, t, true)
    u.y = push_stack(map, t)
  elseif u_name == "set_local" or u_name == "set_upvalue" then
    u.x = pop_stack(map, t, u.store)
  elseif u_name == "set_field" then
    u.x = get_stack(map, a, not u.string_key)
    u.y = get_stack(map, b, not u.string_key)
    u.z = pop_stack(map, t, true)
  elseif u_name == "set_table" then
    u.x = get_stack(map, a, not u.string_key)
    u.y = pop_stack(map, t - 1, not u.string_key)
    u.z = pop_stack(map, t, true)
  elseif u_name == "get_table" then
    u.x = pop_stack(map, t - 1)
    u.y = pop_stack(map, t)
    u.z = push_stack(map, t - 1)
  elseif u_name == "return" then
    u.x = pop_stack_range(map, 1, t)
  elseif u_name == "call" then
    u.x = pop_stack(map, a, true)
    u.y = pop_stack_range(map, a + 1, t)
    if b > 0 then
      u.z = push_stack_range(map, a, a + b - 1)
    end
  elseif u_name == "self" then
    u.x = pop_stack(map, a, true)
    u.y = pop_stack(map, a + 1, true)
    u.z = pop_stack_range(map, a + 2, t)
    if b > 0 then
      u.w = push_stack_range(map, a, a + b - 1)
    end
  elseif u_name == "vararg" then
    if a > 0 then
      u.x = push_stack_range(map, t + 1, t + a)
    end
  elseif u_name == "set_list" then
    u.x = get_stack(map, a)
    u.y = pop_stack_range(map, a + 1, t)
  elseif u_name == "push_nil" then
    u.x = push_stack_range(map, t + 1, t + a)
  elseif u_name == "pop" then
    u.x = pop_stack_range(map, t - a + 1, t)
  end
end

local function boxed(v)
  return v.updef or v.def and v.upuse
end

local function box_env(chunk, value)
  if boxed(chunk.env) then
    return "[" .. value .. "]"
  else
    return value
  end
end

local function box_local(proto, var, value)
  if boxed(proto.locals[var]) then
    return "[" .. value .. "]"
  else
    return value
  end
end

local function unbox_local(proto, var)
  if boxed(proto.locals[var]) then
    return "V" .. var .. "[0]"
  else
    return "V" .. var
  end
end

local function unbox_upvalue(proto, var)
  if boxed(proto.upvalues[var].v) then
    return "U" .. var .. "[0]"
  else
    return "U" .. var
  end
end

local function use(map, n)
  return assert(map[n].v)
end

local function use_range_array(map, range, top)
  local result = {}
  if top == 0 then
    append(result, "S0")
  elseif top > 0 then
    append(result, "[")
    for i, n in ipairs(range) do
      if i > 1 then
        append(result, ",")
      end
      append(result, use(map, n))
    end
    append(result, "]")
  elseif #range == 0 then
    append(result, "S")
  else
    append(result, "[")
    for i, n in ipairs(range) do
      append(result, use(map, n), ",")
    end
    append(result, "...S]")
  end
  return table.concat(result)
end

local function use_range_tuple(map, range, top)
  local result = {}
  if top >= 0 then
    for i, n in ipairs(range) do
      if i > 1 then
        append(result, ",")
      end
      append(result, use(map, n))
    end
  elseif #range == 0 then
    append(result, "...S")
  else
    for i, n in ipairs(range) do
      append(result, use(map, n), ",")
    end
    append(result, "...S")
  end
  return table.concat(result)
end

local function def(result, map, n, v)
  local t = map[n]
  assert(not t.v)

  if t.use == 1 and not t.store and v then
    t.v = v
  else
    local m = map.m + 1
    map.m = m
    t.v = "R" .. m
    append(result, "R", m, "=")
    if v then
      append(result, v, ";")
    end
  end
end

local function def_range(result, map, range, name)
  for i, n in ipairs(range) do
    local t = map[n]
    assert(not t.v)
    local m = map.m + 1
    map.m = m
    t.v = "R" .. m
    append(result, "R", m, "=", name, "[", i - 1, "];")
  end
end

local function generate_code(result, chunk, proto, map, u)
  local u_name = u[0]
  local a = u.a
  local b = u.b
  local t = u.top
  local n = #result

  if u_name == "break" then
    append(result, "break;")

  elseif u_name == "if" then
    local x = use(map, u.x)
    append(result, "if(" .. x .. "!==undefined&&" .. x .. "!==false){\n")
    for _, v in ipairs(u[1]) do
      generate_code(result, chunk, proto, map, v)
    end
    append(result, "}else{\n")
    for _, v in ipairs(u[2]) do
      generate_code(result, chunk, proto, map, v)
    end
    append(result, "}")

  elseif u_name == "loop" then
    append(result, "while(true){\n")
    for _, v in ipairs(u) do
      generate_code(result, chunk, proto, map, v)
    end
    append(result, "}")

  elseif u_name == "check_for" then
    append(result, "V", a, "=OP_CHECKNUMBER(V", a, [[,"bad 'for' initial value");]])
    append(result, "V", a + 1, "=OP_CHECKNUMBER(V", a + 1, [[,"bad 'for' limit");]])
    append(result, "V", a + 2, "=OP_CHECKNUMBER(V", a + 2, [[,"bad 'for' step");]])
    append(result, "if(V", a + 2, [[===0)throw new LuaError("'for' step is zero");]])

  elseif u_name == "add" then
    def(result, map, u.z, "(+" .. use(map, u.x) .. "+ +" .. use(map, u.y) .. ")")

  elseif u_name == "sub" then
    def(result, map, u.z, "(" .. use(map, u.x) .. "-" .. use(map, u.y) .. ")")

  elseif u_name == "mul" then
    def(result, map, u.z, "(" .. use(map, u.x) .. "*" .. use(map, u.y) .. ")")

  elseif u_name == "div" then
    def(result, map, u.z, "(" .. use(map, u.x) .. "/" .. use(map, u.y) .. ")")

  elseif u_name == "idiv" then
    def(result, map, u.z, "Math.floor(" .. use(map, u.x) .. "/" .. use(map, u.y) .. ")")

  elseif u_name == "mod" then
    local y = use(map, u.y)
    def(result, map, u.z, "((" .. use(map, u.x) .. "%" .. y .. "+" .. y .. ")%" .. y .. ")")

  elseif u_name == "pow" then
    def(result, map, u.z, "Math.pow(" .. use(map, u.x) .. "," .. use(map, u.y) .. ")")

  elseif u_name == "band" then
    def(result, map, u.z, "(" .. use(map, u.x) .. "&" .. use(map, u.y) .. ")")

  elseif u_name == "bxor" then
    def(result, map, u.z, "(" .. use(map, u.x) .. "^" .. use(map, u.y) .. ")")

  elseif u_name == "bor" then
    def(result, map, u.z, "(" .. use(map, u.x) .. "|" .. use(map, u.y) .. ")")

  elseif u_name == "shr" then
    def(result, map, u.z, "(" .. use(map, u.x) .. ">>>" .. use(map, u.y) .. ")")

  elseif u_name == "shl" then
    def(result, map, u.z, "(" .. use(map, u.x) .. "<<" .. use(map, u.y) .. ")")

  elseif u_name == "concat" then
    def(result, map, u.z, '(""+' .. use(map, u.x) .. "+" .. use(map, u.y) .. ")")

  elseif u_name == "lt" then
    def(result, map, u.z, "(" .. use(map, u.x) .. "<" .. use(map, u.y) .. ")")

  elseif u_name == "le" then
    def(result, map, u.z, "(" .. use(map, u.x) .. "<=" .. use(map, u.y) .. ")")

  elseif u_name == "gt" then
    def(result, map, u.z, "(" .. use(map, u.x) .. ">" .. use(map, u.y) .. ")")

  elseif u_name == "ge" then
    def(result, map, u.z, "(" .. use(map, u.x) .. ">=" .. use(map, u.y) .. ")")

  elseif u_name == "eq" then
    def(result, map, u.z, "(" .. use(map, u.x) .. "===" .. use(map, u.y) .. ")")

  elseif u_name == "ne" then
    def(result, map, u.z, "(" .. use(map, u.x) .. "!==" .. use(map, u.y) .. ")")

  elseif u_name == "unm" then
    def(result, map, u.y, "(-" .. use(map, u.x) .. ")")

  elseif u_name == "not" then
    local x = use(map, u.x)
    def(result, map, u.y, "(" .. x .. "===undefined||" .. x .. "===false)")

  elseif u_name == "len" then
    def(result, map, u.y, "OP_LEN(" .. use(map, u.x) .. ")")

  elseif u_name == "bnot" then
    def(result, map, u.y, "(~" .. use(map, u.x) .. ")")

  elseif u_name == "new_local" then
    append(result, "V", a, "=", box_local(proto, a, use(map, u.x)), ";")

  elseif u_name == "tbc_local" then
    assert(not boxed(proto.locals[a]))
    append(result, "V", a, "=", use(map, u.x), ";")

  elseif u_name == "set_local" then
    append(result, unbox_local(proto, a), "=", use(map, u.x), ";")

  elseif u_name == "set_upvalue" then
    append(result, unbox_upvalue(proto, a), "=", use(map, u.x), ";")

  elseif u_name == "set_field" or u_name == "set_table" then
    local x = use(map, u.x)
    local y = use(map, u.y)
    local z = use(map, u.z)
    if u.string_key then
      append(result, z, "!==undefined?", x, ".set(", y, ",", z, "):", x, ".delete(", y, ");")
    else
      local cond = x .. ".n!==undefined&&Number.isInteger(" .. y .. ")&&"
      append(result, "if(", z, "!==undefined){if(", cond, y, ">", x, ".n&&", y, "!==++", x, ".n)", x, ".n=undefined;", x, ".set(", y, ",", z, ");}")
      append(result, "else{if(", cond, y, "!==", x, ".n--)", x, ".n=undefined;", x, ".delete(", y, ");}")
    end

  elseif u_name == "get_local" then
    def(result, map, u.x, unbox_local(proto, a))

  elseif u_name == "get_upvalue" then
    def(result, map, u.x, unbox_upvalue(proto, a))

  elseif u_name == "get_table" then
    def(result, map, u.z, "OP_GETTABLE(" .. use(map, u.x) .. "," .. use(map, u.y) .. ")")

  elseif u_name == "new_table" then
    def(result, map, u.x, "new LuaTable()")

  elseif u_name == "closure" then
    local buffer = { "P", a, "(" }
    for i, v in ipairs(chunk[a].upvalues) do
      if i > 1 then
        append(buffer, ",")
      end
      if v.var < 0 then
        append(buffer, "U", -v.var)
      else
        append(buffer, "V", v.var)
      end
    end
    append(buffer, ")")
    def(result, map, u.x, table.concat(buffer))

  elseif u_name == "push_false" then
    def(result, map, u.x, "false")

  elseif u_name == "push_true" then
    def(result, map, u.x, "true")

  elseif u_name == "push_literal" then
    def(result, map, u.x, quote_js(a))

  elseif u_name == "push_numeral" then
    if b == "HexadecimalFloatingNumeral" then
      compiler_error("not supported: push_numeral " .. a .. " HexadecimalFloatingNumeral", u.node)
    else
      def(result, map, u.x, a)
    end

  elseif u_name == "close" then
    append(result, "OP_CLOSE(V", a, ");V", a, "=undefined;")

  elseif u_name == "return" then
    append(result, "return ", use_range_array(map, u.x, t), ";")

  elseif u_name == "call" then
    local x = use(map, u.x)
    local y = use_range_tuple(map, u.y, t)
    local xy = y == "" and x or x .. "," .. y
    if b == 0 then
      append(result, "!", x, ".LuaTable?", x, "(", y, "):", x, '.metatable.get("__call")(', xy, ");")
    elseif b == 1 then
      def(result, map, u.z[1])
      append(result, x, ".LuaFunction?", x, "(", y, ")[0]:!", x, ".LuaTable?", x, "(", y, "):", x, '.metatable.get("__call")(', xy, ")[0];")
    else
      append(result, "S=", x, ".LuaFunction?", x, "(", y, "):!", x, ".LuaTable?[", x, "(", y, ")]:", x, '.metatable.get("__call")(', xy, ");")
      if b > 1 then
        def_range(result, map, u.z, "S")
      end
    end

  elseif u_name == "self" then
    local x = use(map, u.x)
    local y = use(map, u.y)
    local z = use_range_tuple(map, u.z, t)
    local xz = z == "" and x or x .. "," .. z
    append(result, y, "=OP_GETTABLE(", x, ",", y, ");")
    if b == 0 then
      append(result, y, ".LuaFunction?", y, "(", xz, "):!", y, ".LuaTable?", y, ".call(", xz, "):", y, '.metatable.get("__call")(', y, ",", xz, ");")
    elseif b == 1 then
      def(result, map, u.w[1])
      append(result, y, ".LuaFunction?", y, "(", xz, ")[0]:!", y, ".LuaTable?", y, ".call(", xz, "):", y, '.metatable.get("__call")(', y, ",", xz, ")[0];")
    else
      append(result, "S=", y, ".LuaFunction?", y, "(", xz, "):!", y, ".LuaTable?[", y, ".call(", xz, ")]:", y, '.metatable.get("__call")(', y, ",", xz, ");")
      if b > 1 then
        def_range(result, map, u.w, "S")
      end
    end

  elseif u_name == "vararg" then
    if a > 0 then
      def_range(result, map, u.x, "VA")
    else
      assert(a == -1)
      append(result, "S=VA;")
    end

  elseif u_name == "set_list" then
    append(result, "OP_SETLIST(", use(map, u.x), ",", use_range_array(map, u.y, t), ");")

  elseif u_name == "push_nil" then
    for _, n in ipairs(u.x) do
      def(result, map, n, "undefined")
    end

  elseif u_name == "pop" then
    -- noop

  else
    compiler_error("not supported: " .. u_name, u.node)
  end

  if #result > n then
    append(result, "\n")
  end
end

local function generate_proto(result, chunk, proto)
  local try_catch
  local map = { m = 0, n = 0, stack = {} }

  for _, v in ipairs(proto.code) do
    process_code(map, v)
  end

  append(result, "const P", proto.index, "=(")
  for i = 1, #proto.upvalues do
    if i > 1 then
      append(result, ",")
    end
    append(result, "U", i)
  end
  append(result, ")=>{const F=(")
  for i = 1, proto.nparams do
    if i > 1 then
      append(result, ",")
    end
    if boxed(proto.locals[i]) then
      append(result, "A", i)
    else
      append(result, "V", i)
    end
  end
  if proto.vararg then
    if proto.nparams > 0 then
      append(result, ",")
    end
    append(result, "...VA")
  end
  append(result, ")=>{")
  if proto.node.f then
    append(result, "// ", proto.node.f, ":", proto.node.n, ":", proto.node.c)
  end
  append(result, "\n")

  append(result, "let S")
  for i, v in ipairs(proto.locals) do
    if i <= proto.nparams then
      if boxed(proto.locals[i]) then
        append(result, ",V", i, "=", box_local(proto, i, "A" .. i))
      end
    else
      append(result, ",V", i)
    end
    if v.attribute == "close" then
      try_catch = true
    end
  end
  local place_holder = append(result, ";\n")

  if try_catch then
    append(result, "try{\n")
  end

  for _, v in ipairs(proto.code) do
    generate_code(result, chunk, proto, map, v)
  end

  if map.m > 0 then
    local buffer = {}
    for i = 1, map.m do
      append(buffer, ",R", i)
    end
    append(buffer, ";\n")
    result[place_holder] = table.concat(buffer)
  end

  if try_catch then
    append(result, "}catch(e){\n")
    for i = #proto.locals, 1, -1 do
      local v = proto.locals[i]
      if v.attribute == "close" then
        append(result, "OP_CLOSE(V", i, ");V", i, "=undefined;\n")
      end
    end
    append(result, "throw e;\n}\n")
  end

  append(result, "return S0;\n};\nF.LuaFunction=true;\nreturn F;\n};\n")
end

local module = {}

function module.generate_prologue(result)
  append(result, stage1_prologue)
end

function module.generate_chunk(result, chunk)
  append(result, "{\n")
  for i = #chunk, 1, -1 do
    generate_proto(result, chunk, chunk[i])
  end
  append(result, "P1(", box_env(chunk, "E"), ")(...A);\n}\n")
end

function module.generate_module(result, name, chunk)
  append(result, "{\n")
  for i = #chunk, 1, -1 do
    generate_proto(result, chunk, chunk[i])
  end
  append(result, 'E.get("package").get("preload").set(', quote_js(name), ",P1(", box_env(chunk, "E"), "));\n}\n")
end

return module
