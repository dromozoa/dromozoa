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

local verbose = os.getenv "VERBOSE" == "1"

local dir = assert(...)

local lua54_regexp = require "dromozoa.compiler.lua54_regexp"
local lua54_parser = require "dromozoa.compiler.lua54_parser"

---------------------------------------------------------------------------

local function compiler_error(message, u)
  if u ~= nil and u.f ~= nil and u.n ~= nil and u.c ~= nil then
    error(u.f .. ":" .. u.n .. ":" .. u.c .. ": compiler error (" .. message .. ")")
  else
    error("compiler error (" .. message .. ")")
  end
end

local function append(t, v)
  assert(v ~= nil)
  local n = #t + 1
  t[n] = v
  return n
end

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
    compiler_error("unknown attribute " .. attribute, u)
  end
  local var = append(scope.proto.locals, { name = name, attribute = attribute, node = u })
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
        if define and (v.attribute == "const" or v.attribute == "close") then
          compiler_error("attempt to assign to const variable " .. name, u)
        end
        return var
      end
    end
    scope = scope.parent
    if not scope then
      return
    end
  until proto ~= scope.proto

  local var = resolve(scope, name, u, define)
  if not var then
    return
  end

  for i, v in ipairs(proto.upvalues) do
    if v.var == var then
      assert(v.name == name)
      return -i
    end
  end
  return -append(proto.upvalues, { name = name, var = var })
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
    compiler_error("label " .. name .. " already defined on line " .. v.node.n, u)
  end
  local label = append(scope.proto.labels, { name = name, node = u })
  append(scope.labels, label)
  return label
end

local function resolve_label(scope, name, u)
  local label = find_label(scope, name)
  if not label then
    compiler_error("no visible label " .. name, u)
  end
  return label
end

---------------------------------------------------------------------------

local opcodes = {
  label     =  0;
  ["break"] =  0;
  ["goto"]  =  0;
  ["if"]    = -1;
  block     =  0;
  loop      =  0;
  check_for =  0;

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

  new_local   = -1;
  tbc_local   = -1;
  set_local   = -1;
  set_upvalue = -1;
  set_field   = -1;
  set_table   = -2;

  get_local   =  1;
  get_upvalue =  1;
  get_table   = -1;

  new_table    = 1;
  closure      = 1;
  push_false   = 1;
  push_true    = 1;
  push_literal = 1;
  push_numeral = 1;

  dup   = 1;
  close = 0;
}

local function append_code(proto, code, u, op, a, b)
  local v = { [0] = op, a = a, b = b, node = u }
  code[#code + 1] = v
  local c = opcodes[op]
  if c then
    proto.top = proto.top + c
  elseif op == "return" then
    proto.top = 0
  elseif op == "call" or op == "self" then
    if b < 0 then
      assert(b == -1)
      proto.top = -a
    else
      proto.top = a + b - 1
    end
  elseif op == "vararg" then
    if a < 0 then
      assert(a == -1)
      proto.top = a - proto.top
    else
      proto.top = proto.top + a
    end
  elseif op == "set_list" then
    proto.top = a
  elseif op == "push_nil" then
    proto.top = proto.top + a
  elseif op == "pop" then
    proto.top = proto.top - a
  else
    error("unknown op " .. op)
  end
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

local function process1(protos, proto, scope, u, loop)
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
      parent = proto;
    }
    proto = u.proto
    proto.index = append(protos, proto)
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
    -- empty statementsは解析の時点でとりのぞかれるので、label文だけがvoid
    -- statementsとして残る。repeat-until文以外のスコープは、スコープの最後の
    -- void statementsの前でスコープを終了する。ブロックの末尾にラベル文があ
    -- るかどうかを検査する。
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
    process1(protos, proto, scope, y, loop)
    -- 内部的に使用する3個の変数を宣言する。
    u.var = declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    process1(protos, proto, scope, x, loop)
    return process1(protos, proto, scope, z, loop)

  elseif u_name == "for_in" then
    -- 制御式の名前解決を先に行う。
    process1(protos, proto, scope, y, loop)
    -- 内部的に使用する4個の変数を宣言する。Lua 5.3以前は3個だったが、Lua 5.4で
    -- to-be-closed変数が追加された。
    u.var = declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u, "close")
    process1(protos, proto, scope, x, loop)
    return process1(protos, proto, scope, z, loop)

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

    -- 左辺に式があれば、式の名前解決を先に行う。
    if y then
      process1(protos, proto, scope, y, loop)
    end
    return process1(protos, proto, scope, x, loop)

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
      compiler_error("break outside loop", u)
    end
    u.target = loop
    -- ジャンプ前の変数リストを記録する。
    u.stack = collect(scope)

  elseif u_name == "goto" or u_name == "return" then
    -- ジャンプ前の変数リストを記録する。
    u.stack = collect(scope)

  elseif u_name == "..." then
    if not proto.vararg then
      compiler_error("cannot use ... outside a vararg function", u)
    end

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
    process1(protos, proto, scope, v, loop)
  end
end

---------------------------------------------------------------------------

local function process2(proto, scope, u, code)
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
      process2(proto, scope, v, code)
    end

    if not scope.repeat_until and not end_of_scope then
      append_close_scope(proto, code, u, scope)
    end

  elseif u_name == "=" then
    process2(proto, scope, x, code)
    local target = proto.top
    process2(proto, scope, y, code)

    for i = #x, 1, -1 do
      local v = x[i]
      if v.var then
        if v.var < 0 then
          append_code(proto, code, u, "set_upvalue", -v.var)
        else
          append_code(proto, code, u, "set_local", v.var)
        end
      else
        append_code(proto, code, u, "set_field", target - 1, target)
        target = target - 2
      end
    end

    if proto.top > 0 then
      append_code(proto, code, u, "pop", proto.top)
    end

  elseif u_name == "label" then
    append_code(proto, code, u, "label", u.label)

  elseif u_name == "break" then
    local v = u.target
    local m = #u.stack
    local n = #v.stack

    assert(m >= n)
    for i = 0, n - 1 do
      assert(u.stack[m - i] == v.stack[n - i])
    end

    append_close_stack(proto, code, u, u.stack, m - n)
    append_code(proto, code, u, "break")

  elseif u_name == "goto" then
    local label = resolve_label(scope, x.v, u)

    local v = proto.labels[label].node
    local m = #u.stack
    local n = #v.stack
    if m <= n then
      for i = 0, n - 1 do
        local var = v.stack[n - i]
        if u.stack[m - i] ~= var then
          compiler_error("<goto " .. x.v .. "> jumps into the scope of local " .. proto.locals[var].name, u)
        end
      end
    end

    append_close_stack(proto, code, u, u.stack, m - n)
    append_code(proto, code, u, "goto", label)

  elseif u_name == "while" then
    local loop_block = append_code(proto, code, u, "loop")
    process2(proto, scope, x, loop_block)
    local then_block, else_block = append_if(proto, loop_block, u)
    process2(proto, scope, y, then_block)
    append_code(proto, else_block, u, "break")

  elseif u_name == "repeat" then
    local loop_block = append_code(proto, code, u, "loop")
    process2(proto, scope, x, loop_block)
    process2(proto, scope, y, loop_block)
    append_close_scope(proto, loop_block, u, scope)
    local then_block = append_if(proto, loop_block, u)
    append_code(proto, then_block, u, "break")

  elseif u_name == "if" or u_name == "elseif" then
    process2(proto, scope, x, code)
    local then_block, else_block = append_if(proto, code, u)
    process2(proto, scope, y, then_block)
    process2(proto, scope, z, else_block)

  elseif u_name == "for" then
    -- Lua 5.2のマニュアルを元に実装する。
    --
    -- Lua 5.3のマニュアルでは、制御変数を計算する位置が変わった。
    --
    -- Lua 5.4のマニュアルでは、制御変数が整数である場合の意味論が変更された。
    -- この変更は必要に応じてOP_CHECK_FORで吸収する。
    -- 1. stepが0の場合にエラーになる。
    -- 2. ラップアラウンドしなくなった。
    --
    -- for v = e1, e2, e3 do block end
    --
    -- do
    --   local var, limit, step = e1, e2, e3
    --   OP_CHECK_FOR(var, limit, step)
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

    process2(proto, scope, y, code)
    append_code(proto, code, u, "new_local", u.var + 2)
    append_code(proto, code, u, "new_local", u.var + 1)
    append_code(proto, code, u, "new_local", u.var)

    append_code(proto, code, u, "check_for", u.var)

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

    process2(proto, scope, z, loop_block)

    append_code(proto, loop_block, u, "get_local", u.var)
    append_code(proto, loop_block, u, "get_local", u.var + 2)
    append_code(proto, loop_block, u, "add")
    append_code(proto, loop_block, u, "set_local", u.var)

  elseif u_name == "exp_2or3" then
    process2(proto, scope, x, code)
    process2(proto, scope, y, code)
    if z then
      process2(proto, scope, z, code)
    else
      append_code(proto, code, u, "push_numeral", "1", "DecimalIntegerNumeral")
    end

  elseif u_name == "for_in" then
    -- Lua 5.3のマニュアルの元にtbcを足して実装する。
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

    process2(proto, scope, y, code)
    append_code(proto, code, u, "tbc_local", u.var + 3)
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

    process2(proto, scope, z, else_block)

  elseif u_name == "function" then
    process2(proto, scope, x, code)
    append_code(proto, code, u, "closure", y.proto.index)
    if x.var then
      if x.var < 0 then
        append_code(proto, code, u, "set_upvalue", -x.var)
      else
        append_code(proto, code, u, "set_local", x.var)
      end
    else
      append_code(proto, code, u, "set_table", 3)
      append_code(proto, code, u, "pop", 1)
    end
    process2(proto, scope, y, code)

  elseif u_name == "local_function" then
    append_code(proto, code, u, "closure", y.proto.index)
    append_code(proto, code, u, "new_local", x.var)
    process2(proto, scope, y, code)

  elseif u_name == "local" then
    process2(proto, scope, x, code)
    if y then
      process2(proto, scope, y, code)
    else
      append_code(proto, code, u, "push_nil", #x)
    end

    for i = #x, 1, -1 do
      local v = x[i]
      if v.attribute == "close" then
        append_code(proto, code, u, "tbc_local", v.var)
      else
        append_code(proto, code, u, "new_local", v.var)
      end
    end

  elseif u_name == "return" then
    process2(proto, scope, x, code)
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
      process2(proto, scope, v, code)
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
    process2(proto, scope, x, code)

  elseif u.binop then
    process2(proto, scope, x, code)
    process2(proto, scope, y, code)
    append_code(proto, code, u, u.binop)

  elseif u.unop then
    process2(proto, scope, x, code)
    append_code(proto, code, u, u.unop)

  elseif u_name == "and" then
    process2(proto, scope, x, code)
    append_code(proto, code, u, "dup")
    local then_block = append_if(proto, code, u)
    append_code(proto, then_block, u, "pop", 1)
    process2(proto, scope, y, then_block)

  elseif u_name == "or" then
    process2(proto, scope, x, code)
    append_code(proto, code, u, "dup")
    local _, else_block = append_if(proto, code, u)
    append_code(proto, else_block, u, "pop", 1)
    process2(proto, scope, y, else_block)

  elseif u_name == "." then
    process2(proto, scope, x, code)
    process2(proto, scope, y, code)
    if not u.define then
      append_code(proto, code, u, "get_table")
    end

  elseif u_name == ":" then
    process2(proto, scope, x, code)
    process2(proto, scope, y, code)

  elseif u_name == "functioncall" then
    local target = proto.top + 1
    process2(proto, scope, x, code)
    process2(proto, scope, y, code)
    if lua54_parser.symbol_names[x[0]] == ":" then
      append_code(proto, code, u, "self", target, u.nr or 1)
    else
      append_code(proto, code, u, "call", target, u.nr or 1)
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
      process2(proto, scope, v, code)
    end
    if proto.top > target then
      append_code(proto, code, u, "set_list", target)
    end

  elseif u_name == "field" then
    process2(proto, scope, x, code)
    if y then
      process2(proto, scope, y, code)
      append_code(proto, code, u, "set_table", u.target)
    end

  elseif u_name == "nil" then
    append_code(proto, code, u, "push_nil", 1)

  elseif u_name == "false" then
    append_code(proto, code, u, "push_false")

  elseif u_name == "true" then
    append_code(proto, code, u, "push_true")

  elseif u_name == "LiteralString" then
    append_code(proto, code, u, "push_literal", u.v)

  elseif u_name == "Numeral" then
    append_code(proto, code, u, "push_numeral", u.v, u.hint)

  elseif u_name == "Name" then
    if u.declare or u.label then
      return
    end

    if not u.resolve then
      append_code(proto, code, u, "push_literal", u.v)
      return
    end

    if not u.var then
      if u.env < 0 then
        append_code(proto, code, u, "get_upvalue", -u.env)
      else
        append_code(proto, code, u, "get_local", u.env)
      end
      append_code(proto, code, u, "push_literal", u.v)
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
      process2(proto, scope, v, code)
    end
  end
end

---------------------------------------------------------------------------

local function process(chunk)
  local protos = {}
  local proto = { locals = {} }
  local scope = { locals = {}, proto = proto }
  declare(scope, "_ENV")
  process1(protos, proto, scope, chunk)
  process2(proto, scope, chunk)
  return protos
end

---------------------------------------------------------------------------

local quotes = {}
for byte = 0x00, 0x1F do
  quotes[string.char(byte)] = ([[\x%02X]]):format(byte)
end
quotes["\b"] = [[\b]]
quotes["\t"] = [[\t]]
quotes["\n"] = [[\n]]
quotes["\v"] = [[\v]]
quotes["\f"] = [[\f]]
quotes["\r"] = [[\r]]

local LS = string.char(0xE2, 0x80, 0xA8)
local PS = string.char(0xE2, 0x80, 0xA9)

local function quote(s)
  return '"' .. s:gsub("[\0-\31\"\\]", quotes):gsub(LS, [[\u2028]]):gsub(PS, [[\u2029]]) .. '"'
end

local function generate_proto_code(out, protos, u, n)
  n = n + 1

  local u_name = u[0]
  local a = u.a
  local b = u.b

  out:write(("  "):rep(n))
  if u_name == "break" then
    out:write "break;"

  elseif u_name == "if" then
    out:write "a=S.pop();"
    out:write "if (a!==undefined&&a!==false) {\n"
    for _, v in ipairs(u[1]) do
      generate_proto_code(out, protos, v, n)
    end
    out:write(("  "):rep(n), "} else {\n")
    for _, v in ipairs(u[2]) do
      generate_proto_code(out, protos, v, n)
    end
    out:write(("  "):rep(n), "}")

  elseif u_name == "loop" then
    out:write "do {\n"
    for _, v in ipairs(u) do
      generate_proto_code(out, protos, v, n)
    end
    out:write(("  "):rep(n), "} while (true);")

  elseif u_name == "add" then out:write "b=S.pop();a=S.pop();S.push(a+b);"
  elseif u_name == "sub" then out:write "b=S.pop();a=S.pop();S.push(a-b);"
  elseif u_name == "mul" then out:write "b=S.pop();a=S.pop();S.push(a*b);"
  elseif u_name == "div" then out:write "b=S.pop();a=S.pop();S.push(a/b);"
  elseif u_name == "idiv" then out:write "b=S.pop();a=S.pop();S.push(Math.floor(a/b));"
  elseif u_name == "mod" then out:write "b=S.pop();a=S.pop();S.push((a%b+b)%b);"
  elseif u_name == "pow" then out:write "b=S.pop();a=S.pop();S.push(Math.pow(a,b));"
  elseif u_name == "band" then out:write "b=S.pop();a=S.pop();S.push(a&b);"
  elseif u_name == "bxor" then out:write "b=S.pop();a=S.pop();S.push(a^b);"
  elseif u_name == "bor" then out:write "b=S.pop();a=S.pop();S.push(a|b);"
  elseif u_name == "shr" then out:write "b=S.pop();a=S.pop();S.push(a>>b);"
  elseif u_name == "shl" then out:write "b=S.pop();a=S.pop();S.push(a<<b);"
  elseif u_name == "concat" then out:write "b=S.pop();a=S.pop();S.push(a.toString()+b.toString());"
  elseif u_name == "lt" then out:write "b=S.pop();a=S.pop();S.push(a<b);"
  elseif u_name == "le" then out:write "b=S.pop();a=S.pop();S.push(a<=b);"
  elseif u_name == "gt" then out:write "b=S.pop();a=S.pop();S.push(a>b);"
  elseif u_name == "ge" then out:write "b=S.pop();a=S.pop();S.push(a>=b);"
  elseif u_name == "eq" then out:write "b=S.pop();a=S.pop();S.push(a===b);"
  elseif u_name == "ne" then out:write "b=S.pop();a=S.pop();S.push(a!==b);"

  elseif u_name == "unm" then out:write "a=S.pop();S.push(-a);"
  elseif u_name == "not" then out:write "a=S.pop();S.push(a===undefined||a===false);"
  elseif u_name == "len" then out:write "b=1;a=S.pop();for(;a.get(b)!==undefined;++b);S.push(b-1);"
  elseif u_name == "bnot" then out:write "a=S.pop();S.push(~a);"

  elseif u_name == "new_local" then
    out:write("V", a, "=[S.pop()];")

  elseif u_name == "tbc_local" then
    -- TODO tbcを実装する
    out:write("V", a, "=[S.pop()];")

  elseif u_name == "set_local" then
    out:write("V", a, "[0]=S.pop();")

  elseif u_name == "set_upvalue" then
    out:write("U", a, "[0]=S.pop();")

  elseif u_name == "set_field" then
    out:write "c=S.pop();"
    out:write("b=S[", b - 1, "];")
    out:write("a=S[", a - 1, "];")
    out:write "a.set(b, c);"

  elseif u_name == "set_table" then
    out:write "c=S.pop();"
    out:write "b=S.pop();"
    out:write("a=S[", a - 1, "];")
    out:write "a.set(b, c);"

  elseif u_name == "get_local" then
    out:write("S.push(V", a, "[0]);")

  elseif u_name == "get_upvalue" then
    out:write("S.push(U", a, "[0]);")

  elseif u_name == "get_table" then
    out:write "b=S.pop();"
    out:write "a=S.pop();"
    out:write "S.push(a.get(b));"

  elseif u_name == "new_table" then
    out:write "S.push(new Map());"

  elseif u_name == "closure" then
    out:write("S.push(P", a, "(")
    for i, v in ipairs(protos[a].upvalues) do
      if i > 1 then
        out:write ", "
      end
      if v.var < 0 then
        out:write("U", -v.var)
      else
        out:write("V", v.var)
      end
    end
    out:write "));"

  elseif u_name == "push_false" then
    out:write "S.push(false);"

  elseif u_name == "push_true" then
    out:write "S.push(true);"

  elseif u_name == "push_literal" then
    out:write("S.push(", quote(a), ");")

  elseif u_name == "push_numeral" then
    -- TODO hexadecimal floatをどうにかする
    out:write("S.push(", a, ");")

  elseif u_name == "dup" then
    out:write "S.push(S[S.length-1]);"

  elseif u_name == "return" then
    out:write "return S"

  elseif u_name == "call" then
    out:write("b=S.splice(", a, ");")
    out:write "a=S.pop();"
    if b == 0 then
      out:write "a(...b);"
    else
      out:write "c=a(...b);"
      if b ~= -1 then
        out:write("if (c.length<", b, ") c[", b - 1, "]=undefined; else c=c.slice(0,", b, ");")
      end
      out:write "S.push(...c);"
    end

  elseif u_name == "self" then
    out:write("c=S.splice(", a + 1, ");")
    out:write "b=S.pop();"
    out:write "a=S.pop();"

    if b == 0 then
      out:write "(a.get(b))(a, ...c);"
    else
      out:write "c=(a.get(b))(...c);"
      if b ~= -1 then
        out:write("if (c.length<", b, ") c[", b - 1, "]=undefined; else c=c.slice(0,", b, ");")
      end
      out:write "S.push(...c);"
    end


  elseif u_name == "set_list" then
    out:write("b=S.splice(", a, ");")
    out:write("a=S[", a - 1, "];")
    out:write("for (let i=0; i<b.length; ++i) a.set(i+1, b[i]);")

  elseif u_name == "push_nil" then
    out:write "S.push("
    for i = 1, a do
      if i > 1 then
        out:write ", "
      end
      out:write "undefined"
    end
    out:write ");"

  elseif u_name == "pop" then
    out:write("S.splice(-", a, ");")

  else
    out:write("/* ", u_name , " */")
  end
  out:write "\n"
end

local function generate_proto(out, protos, proto)
  out:write("const P", proto.index, " = (")
  for i = 1, #proto.upvalues do
    if i > 1 then
      out:write ", "
    end
    out:write("U", i)
  end
  out:write ") => {\n"
  out:write "  return ("
  for i = 1, proto.nparams do
    if i > 1 then
      out:write ", "
    end
    out:write("A", i)
  end
  if proto.vararg then
    if proto.nparams > 0 then
      out:write ", "
    end
    out:write "...VA"
  end
  out:write ") => {\n"

  out:write "    let S=[], a, b, c;\n"
  for i = 1, #proto.locals do
    out:write("    let V", i)
    if i <= proto.nparams then
      out:write("=[A", i, "]")
    end
    out:write ";\n"
  end

  for _, v in ipairs(proto.code) do
    generate_proto_code(out, protos, v, 1)
  end

  out:write "  };\n"
  out:write "};\n"
end

local function generate_chunk(out)
  out:write [[
const fs = require("fs");
const io = new Map();
io.set("write", (s) => {
  fs.writeSync(1, s);
});
const env = new Map();
env.set("io", io);
const chunk = P1([env]);
chunk(...process.argv.slice(2));
]]
end

---------------------------------------------------------------------------

local quotes = {
  ['&'] = '&amp;';
  ['<'] = '&lt;';
  ['>'] = '&gt;';
  ['"'] = '&quot;';
}

local function quote(s)
  return '"' .. string.gsub(s, '[&<>"]', quotes) .. '"'
end

local function dump_attrs(out, u, attrs)
  for _, attr in ipairs(attrs) do
    local v = u[attr]
    if v ~= nil then
      local t = type(v)
      if t == "boolean" or t == "number" or t == "string" then
        out:write(" ", attr, "=", quote(tostring(v)))
      elseif #v > 0 then
        out:write(" ", attr, "=", quote(table.concat(v, ",")))
      end
    end
  end
end

local node_attrs = {
  "v";
  "declare", "resolve", "define", "label";
  "var", "env";
  "adjust", "nr";
  "loop";
  "self", "vararg";
  "attribute";
  "binop", "unop";
  "hint";
  "end_of_scope";
  "stack";
}

local function dump_node(out, u, n)
  if n == nil then
    n = 0
  else
    n = n + 1
  end

  out:write(("  "):rep(n), "<node")
  if u[0] ~= nil then
    out:write(" name=", quote(lua54_parser.symbol_names[u[0]]))
  end
  dump_attrs(out, u, node_attrs)

  if #u == 0 then
    out:write "/>\n"
  else
    out:write ">\n"
    for _, v in ipairs(u) do
      dump_node(out, v, n)
    end
    out:write(("  "):rep(n), "</node>\n")
  end
end

local function dump_proto_list(out, list, list_name, name)
  if #list == 0 then
    out:write("    <", list_name, "/>\n")
  else
    out:write("    <", list_name, ">\n")
    for i, v in ipairs(list) do
      out:write("      <", name, " index=\"", i, "\"")
      dump_attrs(out, v, { "name", "attribute", "var" })
      if v.node then
        dump_attrs(out, v.node, { "n", "c" })
      end
      out:write "/>\n"
    end
    out:write("    </", list_name, ">\n")
  end
end

local function dump_proto_code(out, u, n)
  if n == nil then
    n = 0
  else
    n = n + 1
  end

  out:write(("  "):rep(n), "<code op=", quote(u[0]))
  if u.a ~= nil then
    out:write(" a=", quote(u.a))
  end
  if u.b ~= nil then
    out:write(" b=", quote(u.b))
  end

  if #u == 0 then
    out:write "/>\n"
  else
    out:write ">\n"
    for _, v in ipairs(u) do
      dump_proto_code(out, v, n)
    end
    out:write(("  "):rep(n), "</code>\n")
  end
end

local function dump_protos(out, protos)
  out:write "<protos>\n"
  for _, proto in ipairs(protos) do
    out:write "  <proto"
    dump_attrs(out, proto, { "index", "nparams", "self", "vararg" })
    out:write ">\n"

    dump_proto_list(out, proto.locals, "locals", "local")
    dump_proto_list(out, proto.upvalues, "upvalues", "upvalue")
    dump_proto_list(out, proto.labels, "labels", "label")

    out:write "    <scopes>\n"
    for _, scope in ipairs(proto.scopes) do
      out:write "      <scope"
      dump_attrs(out, scope, { "index", "repeat_until", "labels", "locals" })
      out:write "/>\n"
    end
    out:write "    </scopes>\n"

    if #proto.code == 0 then
      out:write "    <codes/>\n"
    else
      out:write "    <codes>\n"
      for _, v in ipairs(proto.code) do
        dump_proto_code(out, v, 2)
      end
      out:write "    </codes>\n"
    end

    out:write "  </proto>\n"
  end
  out:write "</protos>\n"
end

for i = 2, #arg do
  local source_filename = assert(arg[i])
  local result_basename = assert(source_filename:match "([^/]+)%.lua$")
  result_basename = dir .. "/" .. result_basename

  local handle = assert(io.open(source_filename))
  local source = handle:read "*a"
  handle:close()

  local out = assert(io.open(result_basename .. "_list.xml", "w"))
  out:write "<nodes>\n"

  local parse = lua54_parser()
  local root = lua54_regexp(source, source_filename, lua54_parser.max_terminal_symbol, function (token)
    dump_node(out, token)
    return parse(token)
  end)

  out:write "</nodes>\n"
  out:close()

  local protos = process(root)

  local out = assert(io.open(result_basename .. "_tree.xml", "w"))
  dump_node(out, root)
  out:close()

  local out = assert(io.open(result_basename .. "_protos.xml", "w"))
  dump_protos(out, protos)
  out:close()

  local out = assert(io.open(result_basename .. ".js", "w"))
  for i = #protos, 1, -1 do
    generate_proto(out, protos, protos[i])
  end
  generate_chunk(out);
  out:close()
end
