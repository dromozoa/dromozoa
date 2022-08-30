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

local array = require "dromozoa.array"
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
  local var = scope.proto.locals:append{name=name, attribute=attribute, node=u}:size()
  scope.locals:append(var)
  return var
end

local function resolve(scope, name, u, define)
  local proto = scope.proto
  repeat
    for i = scope.locals:size(), 1, -1 do
      local var = scope.locals:get(i)
      local v = proto.locals:get(var)
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

  for i, v in proto.upvalues:ipairs() do
    if v.var == var then
      assert(v.name == name)
      return -i
    end
  end
  return -proto.upvalues:append{name=name, var=var}:size()
end

local function collect(scope)
  local stack = array()
  local proto = scope.proto
  repeat
    for i = scope.locals:size(), 1, -1 do
      stack:append(scope.locals:get(i))
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

local codes = {
  label     =  0;
  ["break"] =  0;
  ["goto"]  =  0;
  ["if"]    = -1;
  block     =  0;
  loop      =  0;
  ["for"]   =  0;

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

  set_local     = -1;
  set_local_tbc = -1;
  set_upvalue   = -1;
  set_field     = -1;
  set_table     = -2;

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
  swap  = 0;
  close = 0;

  -- call t nr
  -- return
  -- vararg nr
  -- set_list t

  -- push_nil n
  -- pop n
}

local function append_code(proto, code, u, op, a, b)
  local v = { [0] = op, a = a, b = b, c = c, node = u }

  code[#code + 1] = v
  local t = codes[op]
  if t then
    proto.top = proto.top + t
  elseif op == "call" then
    assert(a > 0)
    local top = a - 1
    if b < 0 then
      assert(b == -1)
      proto.top = b - top
    else
      assert(b >= 0)
      proto.top = top + b
    end
  elseif op == "return" then
    proto.top = 0
  elseif op == "vararg" then
    if a < 0 then
      assert(proto.top >= 0)
      assert(a == -1)
      proto.top = a - proto.top
    else
      assert(a > 0)
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

---------------------------------------------------------------------------

local function process1(protos, proto, scope, u, loop)
  local u_name = lua54_parser.symbol_names[u[0]]
  local x = u[1]
  local y = u[2]
  local z = u[3]

  if u.proto then
    u.proto = {
      vararg = u.vararg;
      self = u.self;
      labels = {};
      locals = array();
      upvalues = array();
      scopes = array();
      code = {};
      top = 0;
      parent = proto;
    }
    proto = u.proto
    proto.index = protos:append(proto):size()
    loop = nil
  end

  if u.scope then
    u.scope = {
      repeat_until = u_name == "repeat";
      labels = {};
      locals = array();
      proto = proto;
      parent = scope;
    }
    scope = u.scope
    scope.index = proto.scopes:append(scope):size()
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
    end

  elseif u_name == "label" then
    u.label = define_label(scope, x.v, u)
    -- goto用に変数リストを記録する。ラベル文がブロックの末尾にある場合、現在の
    -- スコープは終了しているので親スコープを調べる。
    if u.end_of_scope then
      if proto == scope.parent.proto then
        u.stack = collect(scope.parent)
      else
        u.stack = array()
      end
    else
      u.stack = collect(scope)
    end

  elseif u_name == "break" then
    if loop == nil then
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
        for j = scope.locals:size(), 1, -1 do
          local var = scope.locals:get(j)
          if proto.locals:get(var).attribute == "close" then
            append_code(proto, code, u, "close", var)
          end
        end
      end
      process2(proto, scope, v, code)
    end

    if not scope.repeat_until and end_of_scope == nil then
      for j = scope.locals:size(), 1, -1 do
        local var = scope.locals:get(j)
        if proto.locals:get(var).attribute == "close" then
          append_code(proto, code, u, "close", var)
        end
      end
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

    local m = u.stack:size()
    local n = v.stack:size()

    assert(m >= n)
    for i = 0, n - 1 do
      assert(u.stack:get(m - i) == v.stack:get(n - i))
    end

    for i = 1, m - n do
      local var = u.stack:get(i)
      if proto.locals:get(var).attribute == "close" then
        append_code(proto, code, u, "close", var)
      end
    end
    append_code(proto, code, u, "break")

  elseif u_name == "goto" then
    local v = u[1]
    u.label = resolve_label(scope, v.v, u)

    local y = proto.labels[u.label].node

    local m = u.stack:size()
    local n = y.stack:size()
    if m <= n then
      for i = 0, n - 1 do
        local var = y.stack:get(n - i)
        if u.stack:get(m - i) ~= var then
          compiler_error("<goto " .. v.v .. "> jumps into the scope of local " .. proto.locals:get(var).name, u)
        end
      end
    end

    for i = 1, m - n do
      local var = u.stack:get(i)
      if proto.locals:get(var).attribute == "close" then
        append_code(proto, code, u, "close", var)
      end
    end
    append_code(proto, code, u, "goto", u.label)

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

    for j = scope.locals:size(), 1, -1 do
      local var = scope.locals:get(j)
      if proto.locals:get(var).attribute == "close" then
        append_code(proto, loop_block, u, "close", var)
      end
    end

    local then_block = append_if(proto, loop_block, u)
    append_code(proto, then_block, u, "break")

  elseif u_name == "if" or u_name == "elseif" then
    process2(proto, scope, x, code)
    local then_block, else_block = append_if(proto, code, u)
    process2(proto, scope, y, then_block)
    process2(proto, scope, z, else_block)

  elseif u_name == "for" then
    process2(proto, scope, y, code)
    append_code(proto, code, u, "set_local", u.var + 2)
    append_code(proto, code, u, "set_local", u.var + 1)
    append_code(proto, code, u, "set_local", u.var)
    local loop_block = append_code(proto, code, u, "for", u.var)
    process2(proto, scope, z, loop_block)

  elseif u_name == "exp_2or3" then
    process2(proto, scope, x, code)
    process2(proto, scope, y, code)
    if z then
      process2(proto, scope, z, code)
    else
      append_code(proto, code, u, "push_numeral", "1", "DecimalIntegerNumeral")
    end

  elseif u_name == "for_in" then
    process2(proto, scope, y, code)
    append_code(proto, code, u, "set_local_tbc", u.var + 3)
    append_code(proto, code, u, "set_local", u.var + 2)
    append_code(proto, code, u, "set_local", u.var + 1)
    append_code(proto, code, u, "set_local", u.var)

    local loop_block = append_code(proto, code, u, "loop")
    append_code(proto, loop_block, u, "get_local", u.var)
    append_code(proto, loop_block, u, "get_local", u.var + 1)
    append_code(proto, loop_block, u, "get_local", u.var + 2)
    append_code(proto, loop_block, u, "call", 1, #x)
    for i = #x, 1, -1 do
      local v = x[i]
      append_code(proto, loop_block, u, "set_local", v.var)
    end

    append_code(proto, loop_block, u, "get_local", u.var + 4)
    append_code(proto, loop_block, u, "push_nil", 1)
    append_code(proto, loop_block, u, "eq")
    local then_block, else_block = append_if(proto, loop_block, u)
    append_code(proto, then_block, u, "close", u.var + 3)
    append_code(proto, then_block, u, "break")
    append_code(proto, else_block, u, "get_local", u.var + 4)
    append_code(proto, else_block, u, "set_local", u.var + 2)

    process2(proto, scope, u[3], else_block)

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
    append_code(proto, code, u, "set_local", x.var)
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
        append_code(proto, code, u, "set_local_tbc", v.var)
      else
        append_code(proto, code, u, "set_local", v.var)
      end
    end

  elseif u_name == "return" then
    process2(proto, scope, x, code)

    for _, var in u.stack:ipairs() do
      if proto.locals:get(var).attribute == "close" then
        append_code(proto, code, u, "close", var)
      end
    end

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
      append_code(proto, code, u, "get_table", 2)
    end

  elseif u_name == ":" then
    process2(proto, scope, x, code)
    append_code(proto, code, u, "dup")
    process2(proto, scope, y, code)
    append_code(proto, code, u, "get_table", 2)
    append_code(proto, code, u, "swap")

  elseif u_name == "functioncall" then
    local target = proto.top + 1
    process2(proto, scope, x, code)
    process2(proto, scope, y, code)
    append_code(proto, code, u, "call", target, u.nr or 1)

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
    append_code(proto, code, u, "set_list", target)

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
        append_code(proto, code, u, "get_table", 2)
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

-- 仮引数の数
-- 可変長引数を持つか
-- ローカル変数の数（仮引数を含む）
--
-- TODO 暗黙のreturnをどうするか検討する
--   chunkとfuncbodyで違う？

-- TODO arrayに依存しないようにする

--[[

   0  close local         TBCを閉じる
   0  label label         ラベルを定義する
   0  break               ループから出る
   0  loop code           ループを定義する
  -1  if code code        条件文を定義する
   0  block               条件文で使うブロックを定義する
  -1  set_local local     スタックトップをローカル変数に保存する
  -1  set_local_tbc local スタックトップをローカル変数に保存する
  +1  get_local local     スタックにローカル変数を積む
  +1  get_upvalue upval   スタックに上位値を積む
  -1  binop
  +1  push                スタックに定数を積む
  +n  push_nil n          スタックにnilをn個積む
  +1  closure proto       スタックにクロージャを積む
  -n  pop n               スタックからn個とりのぞく
   0  goto label          ジャンプする

  -1  get_table           k=pop(), t=pop() push(t[k])
  -1  set_field t k       t[k]=pop()
  -2  set_table t         v=pop() k=pop() t[k]=v

      call f nresults     スタックトップまでを引数として呼ぶ
      return              スタックトップまでを引数として返す
      vararg nresults

      set_list t          スタックトップまでをリストとしてテーブルに設定する

      dup                 スタックトップを複製する
      swap                スタックトップとその下の要素を交換する

    0 for local code      数値forループの特殊命令

]]

local function process(chunk)
  local protos = array()
  local proto = { locals = array() }
  local scope = { locals = array(), proto = proto }
  declare(scope, "_ENV")
  process1(protos, proto, scope, chunk)
  process2(proto, scope, chunk)
  return protos
end

---------------------------------------------------------------------------

local function quote(s)
  return '"' .. string.gsub(s, '[&<>"]', { ['&'] = '&amp;', ['<'] = '&lt;', ['>'] = '&gt;', ['"'] = '&quot;' }) .. '"'
end

local attrs = {
  "v";
  "attribute";
  "declare", "resolve", "define", "var", "env";
  "define_label", "resolve_label", "label";
  "adjust", "nomultret", "nr", "ns", "push", "pop";
  "top";
}
if verbose then
  for _, attr in ipairs{"i", "j", "f", "n", "c", "s"} do
    attrs[#attrs + 1] = attr
  end
end

local function dump_attrs(out, u, attrs)
  for _, attr in ipairs(attrs) do
    local v = u[attr]
    if v ~= nil then
      local t = type(v)
      out:write(" ", attr, "=")
      if t == "boolean" or t == "number" or t == "string" then
        out:write(quote(tostring(v)))
      else
        out:write(quote(t))
      end
    end
  end
end

local function dump_code(out, u, n)
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
      dump_code(out, v, n)
    end
    out:write(("  "):rep(n), "</code>\n")
  end
end

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
  dump_attrs(out, u, attrs)

  local code = u.code

  if #u == 0 and u.code == nil then
    out:write "/>\n"
  else
    out:write ">\n"

    for _, v in ipairs(u) do
      dump_node(out, v, n)
    end

    if u.code ~= nil then
      for _, v in ipairs(u.code) do
        dump_code(out, v, n)
      end
    end

    out:write(("  "):rep(n), "</node>\n")
  end
end

local function dump_protos(out, protos)
  out:write "<protos>\n"
  for i, proto in protos:ipairs() do
    out:write "  <proto"
    dump_attrs(out, proto, {"index", "self", "vararg"})
    out:write ">\n"

    if next(proto.labels) == nil then
      out:write "    <labels/>\n"
    else
      out:write "    <labels>\n"
      for j, v in ipairs(proto.labels) do
        out:write("      <label index=\"", j, "\"")
        dump_attrs(out, v, {"name"})
        if v.node ~= nil then
          dump_attrs(out, v.node, {"n", "c"})
        end
        out:write "/>\n"
      end
      out:write "    </labels>\n"
    end

    if proto.locals:empty() then
      out:write "    <locals/>\n"
    else
      out:write "    <locals>\n"
      for j, v in proto.locals:ipairs() do
        out:write("      <local index=\"", j, "\"")
        dump_attrs(out, v, {"name", "attribute"})
        if v.node ~= nil then
          dump_attrs(out, v.node, {"n", "c"})
        end
        out:write "/>\n"
      end
      out:write "    </locals>\n"
    end

    if proto.upvalues:empty() then
      out:write "    <upvalues/>\n"
    else
      out:write "    <upvalues>\n"
      for j, v in proto.upvalues:ipairs() do
        out:write("      <upvalue index=\"", j, "\"")
        dump_attrs(out, v, {"name", "var"})
        out:write "/>\n"
      end
      out:write "    </upvalues>\n"
    end

    if proto.scopes:empty() then
      out:write "    <scopes/>\n"
    else
      out:write "    <scopes>\n"
      for j, scope in proto.scopes:ipairs() do
        if scope.locals:empty() then
          out:write("      <scope index=\"", j, "\"/>\n")
        else
          out:write("      <scope index=\"", j, "\">\n")
          for _, v in scope.locals:ipairs() do
            out:write("        <local index=\"", v, "\"/>\n")
          end
          out:write "      </scope>\n"
        end
      end
      out:write "    </scopes>\n"
    end

    if proto.code ~= nil then
      for _, v in ipairs(proto.code) do
        dump_code(out, v, 1)
      end
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
end
