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
local gen1_preamble = require "dromozoa.compiler.gen1_preamble"

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
  push_nil     = false;
  push_false   = 1;
  push_true    = 1;
  push_literal = 1;
  push_numeral = 1;
  new_table    = 1;
  closure      = 1;
  pop          = false;

  get_local   = 1;
  get_upvalue = 1;
  get_table   = false;

  add    = -1;
  sub    = -1;
  mul    = -1;
  div    = -1;
  mod    =  false;
  idiv   = -1;
  pow    = -1;
  band   = -1;
  bxor   = -1;
  bor    = -1;
  shr    = -1;
  shl    = -1;
  concat = -1;
  lt     = -1;
  le     = -1;
  gt     = -1;
  ge     = -1;
  eq     = -1;
  ne     = -1;

  unm     = 0;
  ["not"] = false;
  len     = 0;
  bnot    = 0;

}

local function process_code(map, u)
  local u_name = u[0]
  local a = u.a
  local b = u.b
  local t = u.top

  local opcode = opcodes[u_name]
  if opcode == 1 then
    u.x = push_stack(map, t + 1)
  elseif opcode == -1 then
    u.x = pop_stack(map, t - 1)
    u.y = pop_stack(map, t)
    u.z = push_stack(map, t - 1)
  elseif opcode == 0 then
    u.x = pop_stack(map, t)
    u.y = push_stack(map, t)

  elseif u_name == "push_nil" then
    u.x = push_stack_range(map, t + 1, t + a)
  elseif u_name == "pop" then
    u.x = pop_stack_range(map, t - a + 1, t)

  elseif u_name == "get_table" then
    u.x = pop_stack(map, t - 1)
    u.y = pop_stack(map, t)
    u.z = push_stack(map, t - 1)

  elseif u_name == "new_local" then
    u.x = pop_stack(map, t)
  elseif u_name == "set_local" or u_name == "set_upvalue" then
    u.x = pop_stack(map, t, u.store)
  elseif u_name == "set_table" then
    if b then
      u.x = pop_stack(map, a)
    else
      u.x = get_stack(map, a)
    end
    u.y = pop_stack(map, t - 1)
    u.z = pop_stack(map, t)
  elseif u_name == "set_field" then
    u.x = get_stack(map, a)
    u.y = get_stack(map, b)
    u.z = pop_stack(map, t, u.store)
  elseif u_name == "set_list" then
    u.x = get_stack(map, a)
    u.y = pop_stack_range(map, a + 1, t)

  elseif u_name == "mod" then
    u.x = pop_stack(map, t - 1)
    u.y = pop_stack(map, t, true)
    u.z = push_stack(map, t - 1)

  elseif u_name == "not" then
    u.x = pop_stack(map, t, true)
    u.y = push_stack(map, t)

  elseif u_name == "if" then
    u.x = pop_stack(map, t)
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

  elseif u_name == "call" then
    u.x = pop_stack(map, a)
    u.y = pop_stack_range(map, a + 1, t)
    if b > 0 then
      u.z = push_stack_range(map, a, a + b - 1)
    end
  elseif u_name == "self" then
    u.x = pop_stack(map, a)
    u.y = pop_stack(map, a + 1)
    u.z = pop_stack_range(map, a + 2, t)
    if b > 0 then
      u.w = push_stack_range(map, a, a + b - 1)
    end
  elseif u_name == "vararg" then
    if a > 0 then
      u.x = push_stack_range(map, t + 1, t + a)
    end
  elseif u_name == "return" then
    u.x = pop_stack_range(map, 1, t)
  end
end

local function boxed(v)
  return v.updef > 0 or v.def > 1 and v.upuse > 0
end

local function box_env(chunk, value)
  if boxed(chunk.env) then
    return "["..value.."]"
  else
    return value
  end
end

local function box_local(proto, var, value)
  if boxed(proto.locals[var]) then
    return "["..value.."]"
  else
    return value
  end
end

local function unbox_local(proto, var)
  if boxed(proto.locals[var]) then
    return "V"..var.."[0]"
  else
    return "V"..var
  end
end

local function unbox_upvalue(proto, var)
  if boxed(proto.upvalues[var].v) then
    return "U"..var.."[0]"
  else
    return "U"..var
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
    t.v = "R"..m
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
    t.v = "R"..m
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
    append(result, "a=", use(map,u.x), ";if(a!==undefined&&a!==false){\n")
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
    if b == 3 then
      append(result, "V", a + 2, "=OP_CHECKNUMBER(V", a + 2, [[,"bad 'for' step");]])
      append(result, "if(V", a + 2, [[===0)throw new LuaError("'for' step is zero");]])
    end

  elseif u_name == "add" then
    def(result, map, u.z, "(+"..use(map, u.x).."+ +"..use(map, u.y)..")")

  elseif u_name == "sub" then
    def(result, map, u.z, "("..use(map, u.x).."-"..use(map, u.y)..")")

  elseif u_name == "mul" then
    def(result, map, u.z, "("..use(map, u.x).."*"..use(map, u.y)..")")

  elseif u_name == "div" then
    def(result, map, u.z, "("..use(map, u.x).."/"..use(map, u.y)..")")

  elseif u_name == "idiv" then
    def(result, map, u.z, "Math.floor("..use(map, u.x).."/"..use(map, u.y)..")")

  elseif u_name == "mod" then
    local y = use(map, u.y)
    def(result, map, u.z, "(("..use(map, u.x).."%"..y.."+"..y..")%"..y..")")

  elseif u_name == "pow" then
    def(result, map, u.z, "Math.pow("..use(map, u.x)..","..use(map, u.y)..")")

  elseif u_name == "band" then
    def(result, map, u.z, "("..use(map, u.x).."&"..use(map, u.y)..")")

  elseif u_name == "bxor" then
    def(result, map, u.z, "("..use(map, u.x).."^"..use(map, u.y)..")")

  elseif u_name == "bor" then
    def(result, map, u.z, "("..use(map, u.x).."|"..use(map, u.y)..")")

  elseif u_name == "shr" then
    def(result, map, u.z, "("..use(map, u.x)..">>>"..use(map, u.y)..")")

  elseif u_name == "shl" then
    def(result, map, u.z, "("..use(map, u.x).."<<"..use(map, u.y)..")")

  elseif u_name == "concat" then
    def(result, map, u.z, "(''+"..use(map, u.x).."+"..use(map, u.y)..")")

  elseif u_name == "lt" then
    def(result, map, u.z, "("..use(map, u.x).."<"..use(map, u.y)..")")

  elseif u_name == "le" then
    def(result, map, u.z, "("..use(map, u.x).."<="..use(map, u.y)..")")

  elseif u_name == "gt" then
    def(result, map, u.z, "("..use(map, u.x)..">"..use(map, u.y)..")")

  elseif u_name == "ge" then
    def(result, map, u.z, "("..use(map, u.x)..">="..use(map, u.y)..")")

  elseif u_name == "eq" then
    def(result, map, u.z, "("..use(map, u.x).."==="..use(map, u.y)..")")

  elseif u_name == "ne" then
    def(result, map, u.z, "("..use(map, u.x).."!=="..use(map, u.y)..")")

  elseif u_name == "unm" then
    def(result, map, u.y, "(-"..use(map, u.x)..")")

  elseif u_name == "not" then
    local x = use(map, u.x)
    def(result, map, u.y, "("..x.."===undefined||"..x.."===false)")

  elseif u_name == "len" then
    def(result, map, u.y, "OP_LEN("..use(map, u.x)..")")

  elseif u_name == "bnot" then
    def(result, map, u.y, "(~"..use(map, u.x)..")")

  elseif u_name == "new_local" then
    append(result, "V", a, "=", box_local(proto, a, use(map, u.x)), ";")

  elseif u_name == "set_local" then
    append(result, unbox_local(proto, a), "=", use(map, u.x), ";")

  elseif u_name == "set_upvalue" then
    append(result, unbox_upvalue(proto, a), "=", use(map, u.x), ";")

  elseif u_name == "set_field" or u_name == "set_table" then
    local x = use(map, u.x)
    local y = use(map, u.y)
    local z = use(map, u.z)
    if u.literal then
      append(result, "a=", z, ";")
      append(result, "if(a!==undefined)", x, ".set(", y, ",a);")
      append(result, "else ", x, ".delete(", y, ");")
    else
      append(result, "a=", x, ";b=", y, ";c=", z, ";")
      append(result, "if(c!==undefined){if(a.n!==undefined&&Number.isInteger(b)&&b>a.n&&b!==++a.n)a.n=undefined;a.set(b,c);}")
      append(result, "else{if(a.n!==undefined&&Number.isInteger(b)&&b!==a.n--)a.n=undefined;a.delete(b);}")
    end

  elseif u_name == "get_local" then
    def(result, map, u.x, unbox_local(proto, a))

  elseif u_name == "get_upvalue" then
    def(result, map, u.x, unbox_upvalue(proto, a))

  elseif u_name == "get_table" then
    def(result, map, u.z, "OP_GETTABLE("..use(map, u.x)..","..use(map, u.y)..")")

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
      compiler_error("not supported: push_numeral "..a.." HexadecimalFloatingNumeral", u.node)
    else
      def(result, map, u.x, a)
    end

  elseif u_name == "close" then
    assert(not boxed(proto.locals[a]))
    append(result, "if(V", a, "!==undefined)V", a, ".metatable.get('__close')(V", a, ");")
    append(result, "V", a, "=undefined;")

  elseif u_name == "return" then
    append(result, "return ", use_range_array(map, u.x, t), ";")

  elseif u_name == "call" then
    local x = use(map, u.x)
    local y = use_range_tuple(map, u.y, t)
    local xy = y == "" and "a" or "a,"..y
    append(result, "a=", x, ";")
    if b == 0 then
      append(result, "if(!a.LuaTable)a(", y, ");else a.metatable.get('__call')(", xy, ");")
    elseif b == 1 then
      def(result, map, u.z[1])
      append(result, "a.LuaFunction?a(", y, ")[0]:!a.LuaTable?a(", y, "):a.metatable.get('__call')(", xy, ")[0];")
    else
      append(result, "S=a.LuaFunction?a(", y, "):!a.LuaTable?[a(", y, ")]:a.metatable.get('__call')(", xy, ");")
      if b > 1 then
        def_range(result, map, u.z, "S")
      end
    end

  elseif u_name == "self" then
    local x = use(map, u.x)
    local y = use(map, u.y)
    local z = use_range_tuple(map, u.z, t)
    local xz = z == "" and "a" or "a,"..z
    append(result, "a=", x, ";b=OP_GETTABLE(a,", y, ");")
    if b == 0 then
      append(result, "if(b.LuaFunction)b(", xz, ");else if(!b.LuaTable)b.call(", xz, ");else b.metatable.get('__call')(b,", xz, ");")
    elseif b == 1 then
      def(result, map, u.w[1])
      append(result, "b.LuaFunction?b(", xz, ")[0]:!b.LuaTable?b.call(", xz, "):b.metatable.get('__call')(b,", xz, ")[0];")
    else
      append(result, "S=b.LuaFunction?b(", xz, "):!b.LuaTable?[b.call(", xz, ")]:b.metatable.get('__call')(b,", xz, ");")
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
    compiler_error("not supported: "..u_name, u.node)
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

  append(result, "let S,a,b,c")
  for i, v in ipairs(proto.locals) do
    if i <= proto.nparams then
      if boxed(proto.locals[i]) then
        append(result, ",V", i, "=", box_local(proto, i, "A"..i))
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
        assert(not boxed(proto.locals[i]))
        append(result, "if(V", i, "!==undefined)V", i, ".metatable.get('__close')(V", i, ");V", i, "=undefined;")
      end
    end
    append(result, "throw e;\n}\n")
  end

  append(result, "return S0;\n};\nF.LuaFunction=true;\nreturn F;\n};\n")
end

local module = {}

function module.generate_preamble(result)
  append(result, gen1_preamble)
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
  append(result, "E.get('package').get('preload').set(", quote_js(name), ",P1(", box_env(chunk, "E"), "));\n}\n")
end

return module
