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
local lua54_parser = require "dromozoa.compiler.lua54_parser"

---------------------------------------------------------------------------

-- scope--
--   |    \
-- scope-- \
--   |    \ \
-- scope-- \ \
--   |    \ \ \
-- scope----proto <= funcbody
--   |        |
-- scope--    |
--   |    \   |
-- scope-- \  |
--   |    \ \ |
-- scope----proto <= funcbody
--   |        |
-- scope--    |
--   |    \   |
-- scope-- \  |
--   |    \ \ |
-- scope----proto <= chunk
--   |        |
-- scope----proto <= external

local function declare(scope, name, u, attribute)
  if attribute and attribute ~= "const" and attribute ~= "close" then
    compiler_error("unknown attribute '"..attribute.."'", u)
  end
  local var = append(scope.proto.locals, {
    name = name;
    attribute = attribute;
    def = 1;
    use = 0;
    updef = 0;
    upuse = 0;
    node = u;
  })
  append(scope.locals, var)
  return var
end

local function resolve(scope, name, u, define)
  local proto = scope.proto
  repeat
    for i = #scope.locals, 1, -1 do
      local var = scope.locals[i]
      local v = proto.locals[var]
      if v.name == name then
        if define then
          if v.attribute == "const" or v.attribute == "close" then
            compiler_error("attempt to assign to const variable '"..name.."'", u)
          end
          v.def = v.def + 1
        else
          v.use = v.use + 1
        end
        return var, v
      end
    end
    scope = scope.parent
    if not scope then
      return
    end
  until proto ~= scope.proto

  local var, v = resolve(scope, name, u, define)
  if not var then
    return
  end

  if define then
    v.updef = v.updef + 1
  else
    v.upuse = v.upuse + 1
  end
  for i, u in ipairs(proto.upvalues) do
    if u.var == var then
      assert(u.name == name)
      return -i, v
    end
  end
  return -append(proto.upvalues, { name = name, var = var, v = v }), v
end

local function collect(scope)
  local stack = {}
  local proto = scope.proto
  repeat
    for i = #scope.locals, 1, -1 do
      append(stack, scope.locals[i])
    end
    scope = scope.parent
  until proto ~= scope.proto
  return stack
end

local function find_label(scope, name)
  local proto = scope.proto
  repeat
    for i, label in ipairs(scope.labels) do
      local v = proto.labels[label]
      if v.name == name then
        return label, v
      end
    end
    scope = scope.parent
  until proto ~= scope.proto
end

local function define_label(scope, name, u)
  local label, v = find_label(scope, name)
  if label then
    compiler_error("label '"..name.."' already defined on line "..v.node.n, u)
  end
  local label = append(scope.proto.labels, { name = name, node = u })
  append(scope.labels, label)
  return label
end

local function resolve_label(scope, name, u)
  local label, v = find_label(scope, name)
  if not label then
    compiler_error("no visible label '"..name.."' for <goto> at line "..u.n, u)
  end
  return label, v
end

---------------------------------------------------------------------------

local opcodes = {
  push_nil     = false;
  push_false   = 1;
  push_true    = 1;
  push_literal = 1;
  push_numeral = 1;
  new_table    = 1;
  closure      = 1;
  pop          = false;

  get_local   =  1;
  get_upvalue =  1;
  get_table   = -1;

  new_local   = -1;
  set_local   = -1;
  set_upvalue = -1;
  set_table   =  false;
  set_field   = -1;
  set_list    =  false;

  add    = -1;
  sub    = -1;
  mul    = -1;
  div    = -1;
  idiv   = -1;
  mod    = -1;
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
  ["not"] = 0;
  len     = 0;
  bnot    = 0;

  ["if"]    = -1;
  block     =  0;
  check_for =  0;
  loop      =  0;
  ["break"] =  0;
  label     =  0;
  ["goto"]  =  0;

  call       = false;
  self       = false;
  vararg     = false;
  ["return"] = false;

  close = 0;
}

local function append_code(proto, code, u, op, a, b)
  local top = proto.top
  local v = { [0] = op, a = a, b = b, top = top, node = u }

  append(code, v)
  local opcode = opcodes[op]
  if opcode then
    top = top + opcode
  elseif op == "push_nil" then
    top = top + a
  elseif op == "pop" then
    top = top - a
  elseif op == "set_table" then
    if b then
      top = top - 3
    else
      top = top - 2
    end
  elseif op == "set_list" then
    top = a
  elseif op == "call" or op == "self" then
    if b < 0 then
      assert(b == -1)
      top = -a
    else
      top = a + b - 1
    end
  elseif op == "vararg" then
    if a < 0 then
      assert(a == -1)
      top = -top - 1
    else
      top = top + a
    end
  elseif op == "return" then
    top = 0
  else
    error("unknown op "..op)
  end

  proto.top = top
  return v
end

local function append_if(proto, code, u)
  local cond = append_code(proto, code, u, "if")
  local then_block = append_code(proto, cond, u, "block")
  local else_block = append_code(proto, cond, u, "block")
  return then_block, else_block
end

local function append_close_scope(proto, code, u, scope)
  for i = #scope.locals, 1, -1 do
    local var = scope.locals[i]
    if proto.locals[var].attribute == "close" then
      append_code(proto, code, u, "close", var)
    end
  end
end

local function append_close_stack(proto, code, u, stack, n)
  for i = 1, n or #stack do
    local var = stack[i]
    if proto.locals[var].attribute == "close" then
      append_code(proto, code, u, "close", var)
    end
  end
end

---------------------------------------------------------------------------

local function process1(chunk, proto, scope, u, loop)
  local u_name = lua54_parser.symbol_names[u[0]]
  local x = u[1]
  local y = u[2]
  local z = u[3]

  if u.proto then
    u.proto = {
      nparams = 0;
      self = u.self;
      vararg = u.vararg;
      locals = {};
      upvalues = {};
      labels = {};
      scopes = {};
      code = {};
      top = 0;
      node = u;
      parent = proto;
    }
    proto = u.proto
    proto.index = append(chunk, proto)
    loop = nil
  end

  if u.scope then
    u.scope = {
      repeat_until = u_name == "repeat";
      locals = {};
      labels = {};
      proto = proto;
      parent = scope;
    }
    scope = u.scope
    scope.index = append(proto.scopes, scope)
  end

  if u.loop then
    loop = u

    -- break用に変数リストを記録する。
    u.stack = collect(scope)
  end

  if u_name == "block" then
    -- empty statementsは構文解析時にとりのぞかれるので、label文だけがvoid
    -- statementsとして残る。repeat-until文以外のスコープは、スコープの最後の
    -- void statementsの前でスコープを終了する。ブロックの末尾にラベル文がある
    -- かどうかを検査する。
    if not scope.repeat_until then
      for i = #u, 1, -1 do
        local v = u[i]
        if lua54_parser.symbol_names[v[0]] == "label" then
          u.end_of_scope = i
          v.end_of_scope = i
        else
          break
        end
      end
    end

  elseif u_name == "for" then
    -- 制御式の名前解決を先に行う。
    process1(chunk, proto, scope, y, loop)
    -- 内部的に使用する3個の変数を宣言する。
    u.var = declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    process1(chunk, proto, scope, x, loop)
    return process1(chunk, proto, scope, z, loop)

  elseif u_name == "for_in" then
    -- 制御式の名前解決を先に行う。
    process1(chunk, proto, scope, y, loop)
    -- 内部的に使用する4個の変数を宣言する。Lua 5.3以前は3個だったが、Lua 5.4で
    -- to-be-closed変数が追加された。
    u.var = declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u, "close")
    process1(chunk, proto, scope, x, loop)
    return process1(chunk, proto, scope, z, loop)

  elseif u_name == "local_function" then
    process1(chunk, proto, scope, x, code)
    local v = proto.locals[x.var]
    v.def = v.def + 1
    return process1(chunk, proto, scope, y, code)

  elseif u_name == "local" then
    local n = 0
    for _, v in ipairs(x) do
      if v.attribute == "close" then
        n = n + 1
        if n > 1 then
          compiler_error("multiple to-be-closed variables in local list", v)
        end
      end
    end

    -- 右辺に式があれば、式の名前解決を先に行う。
    if y then
      process1(chunk, proto, scope, y, loop)
    end
    return process1(chunk, proto, scope, x, loop)

  elseif u_name == "funcbody" then
    -- colon syntaxで関数が定義されたら、暗黙の仮引数selfを宣言する。
    if proto.self then
      u.var = declare(scope, "self", u)
      proto.nparams = #x + 1
    else
      proto.nparams = #x
    end

  elseif u_name == "label" then
    u.label = define_label(scope, x.v, u)
    -- goto用に変数リストを記録する。ラベル文がブロックの末尾にある場合、現在の
    -- スコープは終了しているので親スコープを調べる。
    if u.end_of_scope then
      if proto == scope.parent.proto then
        u.stack = collect(scope.parent)
      else
        u.stack = {}
      end
    else
      u.stack = collect(scope)
    end

  elseif u_name == "break" then
    if not loop then
      compiler_error("break outside loop at line "..u.n, u)
    end
    u.target = loop
    -- ジャンプ前の変数リストを記録する。
    u.stack = collect(scope)

  elseif u_name == "goto" or u_name == "return" then
    -- ジャンプ前の変数リストを記録する。
    u.stack = collect(scope)

  elseif u_name == "..." then
    if not proto.vararg then
      compiler_error("cannot use '...' outside a vararg function near '...'", u)
    end

  elseif u_name == "and" or u_name == "or" then
    -- 内部変数を使用して、短絡演算子のスタック操作を単一代入にする。
    u.var = declare(scope, "(short-circuit)", u)

  elseif u_name == "Name" then
    if u.declare then
      u.var = declare(scope, u.v, u, u.attribute)
    elseif u.resolve then
      local var = resolve(scope, u.v, u, u.define)
      if var then
        u.var = var
      else
        u.env = resolve(scope, "_ENV")
      end
    end
  end

  for _, v in ipairs(u) do
    process1(chunk, proto, scope, v, loop)
  end
end

---------------------------------------------------------------------------

local function process2(chunk, proto, scope, u, code)
  local u_name = lua54_parser.symbol_names[u[0]]
  local x = u[1]
  local y = u[2]
  local z = u[3]

  if u.proto then
    proto = u.proto
    code = proto.code
  end

  if u.scope then
    scope = u.scope
  end

  if u_name == "block" then
    local end_of_scope = u.end_of_scope
    for i, v in ipairs(u) do
      if end_of_scope == i then
        append_close_scope(proto, code, u, scope)
      end
      process2(chunk, proto, scope, v, code)
    end

    if not scope.repeat_until and not end_of_scope then
      append_close_scope(proto, code, u, scope)
    end

  elseif u_name == "=" then
    process2(chunk, proto, scope, x, code)
    local target = proto.top
    process2(chunk, proto, scope, y, code)

    local n = #x
    if n == 1 then
      local v = x[1]
      if v.var then
        if v.var < 0 then
          append_code(proto, code, u, "set_upvalue", -v.var)
        else
          append_code(proto, code, u, "set_local", v.var)
        end
      else
        append_code(proto, code, u, "set_table", 1, true)
      end
    else
      for i = n, 1, -1 do
        local v = x[i]
        local c
        if v.var then
          if v.var < 0 then
            c = append_code(proto, code, u, "set_upvalue", -v.var)
          else
            c = append_code(proto, code, u, "set_local", v.var)
          end
        else
          c = append_code(proto, code, u, "set_field", target - 1, target)
          c.literal = v.literal
          target = target - 2
        end
        c.store = i < n
      end
      if proto.top > 0 then
        append_code(proto, code, u, "pop", proto.top)
      end
    end
    assert(proto.top == 0)

  elseif u_name == "label" then
    append_code(proto, code, u, "label", u.label)

  elseif u_name == "break" then
    local v = u.target
    local m = #u.stack
    local n = #v.stack

    -- ジャンプ後の変数リストがジャンプ前の変数リストの部分であることを確認する。
    assert(m >= n)
    for i = 0, n - 1 do
      assert(u.stack[m - i] == v.stack[n - i])
    end

    append_close_stack(proto, code, u, u.stack, m - n)
    append_code(proto, code, u, "break")

  elseif u_name == "goto" then
    local label, to = resolve_label(scope, x.v, u)
    local v = to.node
    local m = #u.stack
    local n = #v.stack

    -- ジャンプ後の変数リストがジャンプ前の変数リストの部分であることを確認する。
    for i = 0, n - 1 do
      local var = v.stack[n - i]
      if u.stack[m - i] ~= var then
        compiler_error("<goto "..x.v.."> at line "..u.n.." jumps into the scope of local '"..proto.locals[var].name.."'", u)
      end
    end
    assert(m >= n)

    append_close_stack(proto, code, u, u.stack, m - n)
    append_code(proto, code, u, "goto", label)

  elseif u_name == "while" then
    local loop_block = append_code(proto, code, u, "loop")
    process2(chunk, proto, scope, x, loop_block)
    local then_block, else_block = append_if(proto, loop_block, u)
    process2(chunk, proto, scope, y, then_block)
    append_code(proto, else_block, u, "break")

  elseif u_name == "repeat" then
    local loop_block = append_code(proto, code, u, "loop")
    process2(chunk, proto, scope, x, loop_block)
    process2(chunk, proto, scope, y, loop_block)
    append_close_scope(proto, loop_block, u, scope)
    local then_block = append_if(proto, loop_block, u)
    append_code(proto, then_block, u, "break")

  elseif u_name == "if" or u_name == "elseif" then
    process2(chunk, proto, scope, x, code)
    local then_block, else_block = append_if(proto, code, u)
    process2(chunk, proto, scope, y, then_block)
    process2(chunk, proto, scope, z, else_block)

  elseif u_name == "for" then
    -- Lua 5.2のマニュアルを元に実装する。
    --
    -- Lua 5.3のマニュアルでは、制御変数を計算する位置が変わった。
    --
    -- Lua 5.4のマニュアルでは、制御変数が整数である場合の意味論が変更された。
    -- 1. stepが0の場合にエラーになる。
    -- 2. ラップアラウンドしなくなった。

    process2(chunk, proto, scope, y, code)
    if y.step then
      -- for v = e1, e2, K do block end
      --
      -- do
      --   local var, limit = e1, e2
      --   var, limit = OP_CHECK_FOR(var, limit)
      --   while true do
      --     if var CMP limit then
      --       break
      --     end
      --     local v = var
      --     block
      --     var = var OP K
      --   end
      -- end

      if y.step == 0 then
        compiler_error("'for' step is zero", y[3])
      end

      append_code(proto, code, u, "new_local", u.var + 1)
      append_code(proto, code, u, "new_local", u.var)
      append_code(proto, code, u, "check_for", u.var, 2)

      local loop_block = append_code(proto, code, u, "loop")

      append_code(proto, loop_block, u, "get_local", u.var)
      append_code(proto, loop_block, u, "get_local", u.var + 1)
      append_code(proto, loop_block, u, y.step_cmp)
      local then_block = append_if(proto, loop_block, u)
      append_code(proto, then_block, u, "break")

      append_code(proto, loop_block, u, "get_local", u.var)
      append_code(proto, loop_block, u, "new_local", u.var + 3)

      process2(chunk, proto, scope, z, loop_block)

      append_code(proto, loop_block, u, "get_local", u.var)
      append_code(proto, loop_block, u, "push_numeral", y.step_v, y.step_hint)
      append_code(proto, loop_block, u, y.step_op)
      append_code(proto, loop_block, u, "set_local", u.var)

    else
      -- for v = e1, e2, e3 do block end
      --
      -- do
      --   local var, limit, step = e1, e2, e3
      --   var, limit, step = OP_CHECK_FOR(var, limit, step)
      --   while true do
      --     if step >= 0 then
      --       if var > limit then
      --         break
      --       end
      --     else
      --       if var < limit then
      --         break
      --       end
      --     end
      --     local v = var
      --     block
      --     var = var + step
      --   end
      -- end

      append_code(proto, code, u, "new_local", u.var + 2)
      append_code(proto, code, u, "new_local", u.var + 1)
      append_code(proto, code, u, "new_local", u.var)
      append_code(proto, code, u, "check_for", u.var, 3)

      local loop_block = append_code(proto, code, u, "loop")

      append_code(proto, loop_block, u, "get_local", u.var + 2)
      append_code(proto, loop_block, u, "push_numeral", "0", "DecimalIntegerNumeral")
      append_code(proto, loop_block, u, "ge")
      local then_block, else_block = append_if(proto, loop_block, u)

      append_code(proto, then_block, u, "get_local", u.var)
      append_code(proto, then_block, u, "get_local", u.var + 1)
      append_code(proto, then_block, u, "gt")
      local then_block = append_if(proto, then_block, u)
      append_code(proto, then_block, u, "break")

      append_code(proto, else_block, u, "get_local", u.var)
      append_code(proto, else_block, u, "get_local", u.var + 1)
      append_code(proto, else_block, u, "lt")
      local then_block = append_if(proto, else_block, u)
      append_code(proto, then_block, u, "break")

      append_code(proto, loop_block, u, "get_local", u.var)
      append_code(proto, loop_block, u, "new_local", u.var + 3)

      process2(chunk, proto, scope, z, loop_block)

      append_code(proto, loop_block, u, "get_local", u.var)
      append_code(proto, loop_block, u, "get_local", u.var + 2)
      append_code(proto, loop_block, u, "add")
      append_code(proto, loop_block, u, "set_local", u.var)
    end

  elseif u_name == "exp_2or3" then
    process2(chunk, proto, scope, x, code)
    process2(chunk, proto, scope, y, code)
    if z then
      -- stepが定数の場合はスタックに積まない。
      if lua54_parser.symbol_names[z[0]] == "Numeral" then
        u.step = tonumber(z.v)
        u.step_cmp = "gt"
        u.step_v = z.v
        u.step_hint = z.hint
        u.step_op = "add"
      elseif z.unop == "unm" and lua54_parser.symbol_names[z[1][0]] then
        u.step = -tonumber(z[1].v)
        u.step_cmp = "lt"
        u.step_v = z[1].v
        u.step_hint = z[1].hint
        u.step_op = "sub"
      else
        process2(chunk, proto, scope, z, code)
      end
    else
      u.step = 1
      u.step_cmp = "gt"
      u.step_v = "1"
      u.step_hint = "DecimalIntegerNumeral"
      u.step_op = "add"
    end

  elseif u_name == "for_in" then
    -- Lua 5.3のマニュアルを元にtbcを足して実装する。
    --
    -- for var_1, ..., var_N in explist do block end
    --
    -- do
    --   local f, s, var, tbc <close> = explist
    --   while true do
    --     local var_1, ..., var_N = f(s, var)
    --     if var_1 == nil then
    --       OP_CLOSE(tbc)
    --       break
    --     end
    --     var = var_1
    --     block
    --   end
    -- end

    process2(chunk, proto, scope, y, code)
    append_code(proto, code, u, "new_local", u.var + 3, true)
    append_code(proto, code, u, "new_local", u.var + 2)
    append_code(proto, code, u, "new_local", u.var + 1)
    append_code(proto, code, u, "new_local", u.var)

    local loop_block = append_code(proto, code, u, "loop")

    append_code(proto, loop_block, u, "get_local", u.var)
    append_code(proto, loop_block, u, "get_local", u.var + 1)
    append_code(proto, loop_block, u, "get_local", u.var + 2)
    append_code(proto, loop_block, u, "call", 1, #x)
    for i = #x, 1, -1 do
      local v = x[i]
      append_code(proto, loop_block, u, "new_local", v.var)
    end

    append_code(proto, loop_block, u, "get_local", u.var + 4)
    append_code(proto, loop_block, u, "push_nil", 1)
    append_code(proto, loop_block, u, "eq")
    local then_block, else_block = append_if(proto, loop_block, u)

    append_code(proto, then_block, u, "close", u.var + 3)
    append_code(proto, then_block, u, "break")

    append_code(proto, else_block, u, "get_local", u.var + 4)
    append_code(proto, else_block, u, "set_local", u.var + 2)

    process2(chunk, proto, scope, z, else_block)

  elseif u_name == "function" then
    process2(chunk, proto, scope, x, code)
    append_code(proto, code, u, "closure", y.proto.index)
    if x.var then
      if x.var < 0 then
        append_code(proto, code, u, "set_upvalue", -x.var)
      else
        append_code(proto, code, u, "set_local", x.var)
      end
    else
      local c = append_code(proto, code, u, "set_table", proto.top - 2, true)
      c.literal = assert(x.literal)
    end
    process2(chunk, proto, scope, y, code)

  elseif u_name == "local_function" then
    -- local f; f = function () body end
    append_code(proto, code, u, "push_nil", 1)
    append_code(proto, code, u, "new_local", x.var)
    append_code(proto, code, u, "closure", y.proto.index)
    append_code(proto, code, u, "set_local", x.var)
    process2(chunk, proto, scope, y, code)

  elseif u_name == "local" then
    process2(chunk, proto, scope, x, code)
    if y then
      process2(chunk, proto, scope, y, code)
    else
      append_code(proto, code, u, "push_nil", #x)
    end

    for i = #x, 1, -1 do
      local v = x[i]
      append_code(proto, code, u, "new_local", v.var, v.attribute == "close")
    end

  elseif u_name == "return" then
    process2(chunk, proto, scope, x, code)
    append_close_stack(proto, code, u, u.stack)
    append_code(proto, code, u, "return")

  elseif u_name == "explist" then
    local push
    local pop

    -- 空の式リストは、空の引数リストを表現する場合にだけ出現し、この場合は調節
    -- を行う必要がない。
    if x then
      local v = u[#u]
      local v_name = lua54_parser.symbol_names[v[0]]
      if u.adjust then
        -- 末尾が関数呼び出し式または可変長引数式である場合、可能な限り戻り値の
        -- 個数の調節を行い、不足分をpush/popで調節する。
        if v_name == "functioncall" or v_name == "..." then
          if #u < u.adjust then
            if not v.nr then
              v.nr = u.adjust - #u + 1
            else
              v.nr = 1
              push = u.adjust - #u
            end
          elseif #u == u.adjust then
            v.nr = 1
          else
            v.nr = 0
            if #u > u.adjust + 1 then
              pop = #u - u.adjust - 1
            end
          end
        else
          if #u < u.adjust then
            push = u.adjust - #u
          elseif #u > u.adjust then
            pop = #u - u.adjust
          end
        end
      else
        -- 末尾が関数呼び出し式または可変長引数式で、丸括弧で囲われていなければ、
        -- 戻り値の個数を調節しない。
        if (v_name == "functioncall" or v_name == "...") and not v.nr then
          v.nr = -1
        end
      end
    end

    for _, v in ipairs(u) do
      process2(chunk, proto, scope, v, code)
    end
    if push then
      append_code(proto, code, u, "push_nil", push)
    elseif pop then
      append_code(proto, code, u, "pop", pop)
    end

  elseif u_name == "..." then
    append_code(proto, code, u, "vararg", u.nr or 1)

  elseif u_name == "functiondef" then
    append_code(proto, code, u, "closure", x.proto.index)
    process2(chunk, proto, scope, x, code)

  elseif u.binop then
    process2(chunk, proto, scope, x, code)
    process2(chunk, proto, scope, y, code)
    append_code(proto, code, u, u.binop)

  elseif u_name == "and" then
    process2(chunk, proto, scope, x, code)
    append_code(proto, code, u, "new_local", u.var)
    append_code(proto, code, u, "get_local", u.var)
    local then_block = append_if(proto, code, u)
    process2(chunk, proto, scope, y, then_block)
    append_code(proto, then_block, u, "set_local", u.var)
    append_code(proto, code, u, "get_local", u.var)

  elseif u_name == "or" then
    process2(chunk, proto, scope, x, code)
    append_code(proto, code, u, "new_local", u.var)
    append_code(proto, code, u, "get_local", u.var)
    local _, else_block = append_if(proto, code, u)
    process2(chunk, proto, scope, y, else_block)
    append_code(proto, else_block, u, "set_local", u.var)
    append_code(proto, code, u, "get_local", u.var)

  elseif u.unop then
    process2(chunk, proto, scope, x, code)
    append_code(proto, code, u, u.unop)

  elseif u_name == "." then
    process2(chunk, proto, scope, x, code)
    process2(chunk, proto, scope, y, code)
    u.literal = y.literal
    if not u.define then
      append_code(proto, code, u, "get_table")
    end

  elseif u_name == ":" then
    process2(chunk, proto, scope, x, code)
    process2(chunk, proto, scope, y, code)

  elseif u_name == "functioncall" then
    local target = proto.top + 1
    process2(chunk, proto, scope, x, code)
    process2(chunk, proto, scope, y, code)
    if lua54_parser.symbol_names[x[0]] == ":" then
      append_code(proto, code, u, "self", target, u.nr or 1)
    else
      append_code(proto, code, u, "call", target, u.nr or 1)
    end

    -- チャンク直下のスコープで、文字列リテラルを引数にrequireを呼んでいる場合、
    -- 静的解決の候補とする。
    if proto.index == 1 and scope.index == 1 and x.env == -1 and x.v == "require" and #y == 1 and lua54_parser.symbol_names[y[1][0]] == "LiteralString" then
      append(chunk.require, y[1].v)
    end

  elseif u_name == "fieldlist" then
    -- 末尾がkey=value形式でなく、関数呼び出し式または可変長引数式で、丸括弧で
    -- 囲われていなければ、戻り値の個数を調節しない。
    if x then
      local field = u[#u]
      if not field[2] then
        local v = field[1]
        local v_name = lua54_parser.symbol_names[v[0]]
        if (v_name == "functioncall" or v_name == "...") and not v.nr then
          v.nr = -1
        end
      end
    end

    append_code(proto, code, u, "new_table")
    local target = proto.top
    for i, v in ipairs(u) do
      v.target = target
      process2(chunk, proto, scope, v, code)
    end
    if proto.top ~= target then
      append_code(proto, code, u, "set_list", target)
    end

  elseif u_name == "field" then
    process2(chunk, proto, scope, x, code)
    if y then
      process2(chunk, proto, scope, y, code)
      local c = append_code(proto, code, u, "set_table", u.target, false)
      c.literal = x.literal
    end

  elseif u_name == "nil" then
    append_code(proto, code, u, "push_nil", 1)

  elseif u_name == "false" then
    append_code(proto, code, u, "push_false")

  elseif u_name == "true" then
    append_code(proto, code, u, "push_true")

  elseif u_name == "LiteralString" then
    append_code(proto, code, u, "push_literal", u.v)
    u.literal = true

  elseif u_name == "Numeral" then
    append_code(proto, code, u, "push_numeral", u.v, u.hint)

  elseif u_name == "Name" then
    if u.declare or u.label then
      return
    end

    if not u.resolve then
      append_code(proto, code, u, "push_literal", u.v)
      u.literal = true
      return
    end

    if not u.var then
      if u.env < 0 then
        append_code(proto, code, u, "get_upvalue", -u.env)
      else
        append_code(proto, code, u, "get_local", u.env)
      end
      append_code(proto, code, u, "push_literal", u.v)
      u.literal = true
      if not u.define then
        append_code(proto, code, u, "get_table")
      end
      return
    end

    if not u.define then
      if u.var < 0 then
        append_code(proto, code, u, "get_upvalue", -u.var)
      else
        append_code(proto, code, u, "get_local", u.var)
      end
    end

  else
    for _, v in ipairs(u) do
      process2(chunk, proto, scope, v, code)
    end
  end
end

---------------------------------------------------------------------------

return function (root)
  local chunk = { require = {} }
  local proto = { locals = {} }
  local scope = { locals = {}, proto = proto }
  declare(scope, "_ENV")
  chunk.env = proto.locals[1]

  process1(chunk, proto, scope, root)
  process2(chunk, proto, scope, root)
  return chunk
end
