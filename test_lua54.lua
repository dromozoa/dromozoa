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
    if scope == nil then
      return
    end
  until proto ~= scope.proto

  local var = resolve(scope, name, u, define)
  if var == nil then
    return
  end

  for i, v in proto.upvalues:ipairs() do
    if v.var == var then
      assert(v.name == name)
      return i + 65536
    end
  end
  return proto.upvalues:append{name=name, var=var}:size() + 65536
end

local function collect(scope)
  local locals = array()
  local proto = scope.proto
  repeat
    for i = scope.locals:size(), 1, -1 do
      locals:append(scope.locals:get(i))
    end
    scope = scope.parent
  until proto ~= scope.proto
  return locals
end

local function find_label(scope, name)
  local proto = scope.proto
  repeat
    for i, label in scope.labels:ipairs() do
      local v = proto.labels:get(label)
      if v.name == name then
        return label, v
      end
    end
    scope = scope.parent
  until proto ~= scope.proto
end

local function define_label(scope, name, u)
  local label, v = find_label(scope, name)
  if label ~= nil then
    compiler_error("label " .. name .. " already defined on line " .. v.node.n, u)
  end
  local label = scope.proto.labels:append{name=name, node=u}:size()
  scope.labels:append(label)
  return label
end

local function resolve_label(scope, name, u)
  local label = find_label(scope, name)
  if label == nil then
    compiler_error("no visible label " .. name, u)
  end
  return label
end

---------------------------------------------------------------------------

local function append_code(self, u, op, a, b)
  local v = { [0] = op, a = a, b = b, c = c, node = u }
  self[#self + 1] = v
  return v
end

---------------------------------------------------------------------------

local function process1(protos, proto, scope, u, loop)
  if u.proto ~= nil then
    u.proto.labels = array()
    u.proto.locals = array()
    u.proto.upvalues = array()
    u.proto.scopes = array()
    u.proto.code = {}
    u.proto.parent = proto
    proto = u.proto
    proto.index = protos:append(proto):size()
    loop = nil
  end

  if u.scope ~= nil then
    u.scope.labels = array()
    u.scope.locals = array()
    u.scope.proto = proto
    u.scope.parent = scope
    scope = u.scope
    scope.index = proto.scopes:append(scope):size()
  end

  if u.loop then
    loop = u
  end

  local u_name = lua54_parser.symbol_names[u[0]]

  -- TODO u.localsじゃなくて、var_stackとかのほうがいいかな？

  if u_name == "for" then
    -- ジャンプ解決用に変数リストを逆順で記録する。
    u.locals = collect(scope)
    -- 制御式の名前解決を先に行う。
    process1(protos, proto, scope, u[2], loop)
    -- 内部的に使用する3個の変数を宣言する。
    u.var = declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    process1(protos, proto, scope, u[1], loop)
    process1(protos, proto, scope, u[3], loop)
  elseif u_name == "for_in" then
    -- ジャンプ解決用に変数リストを逆順で記録する。
    u.locals = collect(scope)
    -- 制御式の名前解決を先に行う。
    process1(protos, proto, scope, u[2], loop)
    -- 内部的に使用する4個の変数を宣言する。Lua 5.3以前は3個だったが、Lua 5.4で
    -- to-be-closed変数が追加された。
    u.var = declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u, "close")
    process1(protos, proto, scope, u[1], loop)
    process1(protos, proto, scope, u[3], loop)
  elseif u_name == "local" then
    local n = 0
    for _, v in ipairs(u[1]) do
      if v.attribute == "close" then
        n = n + 1
        if n > 1 then
          compiler_error("multiple to-be-closed variables in local list", v)
        end
      end
    end
    -- 左辺に式があれば、式の名前解決を先に行う。
    if u[2] ~= nil then
      process1(protos, proto, scope, u[2], loop)
    end
    process1(protos, proto, scope, u[1], loop)
  elseif u_name == "funcbody" then
    -- colon syntaxで関数が定義されたら、暗黙の仮引数selfを宣言する。
    if proto.self then
      u.var = declare(scope, "self", u)
    end
    process1(protos, proto, scope, u[1], loop)
    process1(protos, proto, scope, u[2], loop)
  else
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
            v[1].end_of_scope = i
          else
            break
          end
        end
      end
    elseif u_name == "break" then
      if loop == nil then
        compiler_error("break outside loop", u)
      end
      -- ジャンプ解決用にbreak対象と変数リストを逆順で記録する。
      u.target = loop
      u.locals = collect(scope)
    elseif u.loop then
      -- ジャンプ解決用に変数リストを逆順で記録する。
      u.locals = collect(scope)
    elseif u_name == "return" then
      -- ジャンプ解決用に変数リストを逆順で記録する。
      u.locals = collect(scope)
    elseif u_name == "..." then
      if not proto.vararg then
        compiler_error("cannot use ... outside a vararg function", u)
      end
    elseif u.declare then
      u.var = declare(scope, u.v, u, u.attribute)
    elseif u.resolve then
      local var = resolve(scope, u.v, u, u.define)
      if var == nil then
        u.env = resolve(scope, "_ENV")
      else
        u.var = var
      end
    elseif u.define_label then
      -- ジャンプ解決用にラベルと変数リストを逆順で記録する。
      u.label = define_label(scope, u.v, u)
      if u.end_of_scope then
        u.locals = collect(scope.parent)
      else
        u.locals = collect(scope)
      end
    elseif u.resolve_label then
      -- ジャンプ解決用に変数リストを逆順で記録する。
      u.locals = collect(scope)
    end

    for _, v in ipairs(u) do
      process1(protos, proto, scope, v, loop)
    end
  end
end

---------------------------------------------------------------------------

local function process2(scope, u, code, top)
  local save_top = top
  local restore_top

  if u.scope ~= nil then
    if u.scope.proto ~= scope.proto then
      code = u.scope.proto.code
      top = 0
      restore_top = true
    end
    scope = u.scope
  end

  -- local function assert() end

  -- print(code, save_top, top)
  assert(top)
  -- print(u.f, u.v, top)
  u.top = top

  if u.resolve_label then
    u.label = resolve_label(scope, u.v, u)
  end

  local u_name = lua54_parser.symbol_names[u[0]]
  local traversed

  if u_name == "block" then
    traversed = true

    local end_of_scope = u.end_of_scope
    for i, v in ipairs(u) do
      if end_of_scope == i then
        for j = scope.locals:size(), 1, -1 do
          local var = scope.locals:get(j)
          if scope.proto.locals:get(var).attribute == "close" then
            append_code(code, u, "close", var)
          end
        end
      end
      top = process2(scope, v, code, top)
    end

    if not scope.repeat_until and end_of_scope == nil then
      for j = scope.locals:size(), 1, -1 do
        local var = scope.locals:get(j)
        if scope.proto.locals:get(var).attribute == "close" then
          append_code(code, u, "close", var)
        end
      end
    end

  elseif u_name == "label" then
    append_code(code, u, "label", u[1].label)

  elseif u_name == "break" then
    local v = u.target

    local m = u.locals:size()
    local n = v.locals:size()

    assert(m >= n)
    for i = 0, n - 1 do
      assert(u.locals:get(m - i) == v.locals:get(n - i))
    end

    for i = 1, m - n do
      local var = u.locals:get(i)
      if scope.proto.locals:get(var).attribute == "close" then
        append_code(code, u, "close", var)
      end
    end
    append_code(code, u, "break")

  elseif u_name == "while" then
    traversed = true

    local loop = append_code(code, u, "loop")

    top = process2(scope, u[1], code, top)

    local cond = append_code(loop, u, "if")
    top = top - 1

    append_code(cond, u, "block")

    top = process2(scope, u[2], code[1], top)

    append_code(cond, u, "block")
    append_code(cond[2], u, "break")

  elseif u_name == "repeat" then
    code = append_code(code, u, "loop")

  elseif u_name == "if" or u_name == "elseif" then
    traversed = true

    top = process2(scope, u[1], code, top)
    assert(top == 1)
    local cond = append_code(code, u, "if")
    top = top - 1
    append_code(cond, u, "block")
    append_code(cond, u, "block")

    top = process2(scope, u[2], cond[1], top)
    top = process2(scope, u[3], cond[2], top)
    assert(top == 0)

  elseif u_name == "for" then
    traversed = true

    top = process2(scope, u[2], code, top)
    append_code(code, u, "set_local", u.var + 2)
    append_code(code, u, "set_local", u.var + 1)
    append_code(code, u, "set_local", u.var)
    top = top - 3
    assert(top == 0)

    local loop = append_code(code, u, "for", u.var)

    -- local cond = append_code(code, u, "if")
    -- append_code(cond, u, "block")
    -- append_code(cond, u, "block")
    -- local loop = append_code(cond[1], u, "loop")

    top = process2(scope, u[3], loop, top)
    assert(top == 0)

    -- | var   | u.var     |
    -- | limit | u.var + 1 |
    -- | step  | u.var + 2 |
    --
    -- var = var + step
    -- if step > 0 then
    --   if var > limit then
    --     break
    --   end
    -- else
    --   if var < limit then
    --     break
    --   end
    -- end

    -- append_code(loop, u, "get_local", u.var)
    -- append_code(loop, u, "get_local", u.var + 2)
    -- append_code(loop, u, "add")
    -- append_code(loop, u, "set_local", u.var)

    -- append_code(loop, u, "get_local", u.var + 2)
    -- append_code(loop, u, "push_numeral", "0", "DecimalIntegerNumeral")
    -- append_code(loop, u, "gt")

    -- local cond = append_code(loop, u, "if")
    -- append_code(cond, u, "block")
    -- append_code(cond, u, "block")

    -- append_code(cond[1], u, "get_local", u.var)
    -- append_code(cond[1], u, "get_local", u.var + 1)
    -- append_code(cond[1], u, "gt")
    -- local cond2 = append_code(cond[1], u, "if")
    -- append_code(cond2, u, "block")
    -- append_code(cond2, u, "block")
    -- append_code(cond2[1], u, "break")

    -- append_code(cond[2], u, "get_local", u.var)
    -- append_code(cond[2], u, "get_local", u.var + 1)
    -- append_code(cond[2], u, "lt")
    -- local cond2 = append_code(cond[2], u, "if")
    -- append_code(cond2, u, "block")
    -- append_code(cond2, u, "block")
    -- append_code(cond2[1], u, "break")

    -- append_code(loop, u, "get_local", u.var)
    -- append_code(loop, u, "set_local", u.var + 3)
    assert(u.var + 3 == u[1].var)

  elseif u_name == "for_in" then
    traversed = true

    top = process2(scope, u[2], code, top)

    append_code(code, u, "set_local_tbc", u.var + 3)
    append_code(code, u, "set_local", u.var + 2)
    append_code(code, u, "set_local", u.var + 1)
    append_code(code, u, "set_local", u.var)
    top = top - 4
    assert(top == 0)

    -- | f   | u.var     |
    -- | s   | u.var + 1 |
    -- | var | u.var + 2 |
    -- | tbc | u.var + 3 |
    -- | v   | u.var + 4 |

    --  while true do
    --    v, ... = f(s, var)
    --    if v == nil then
    --      break
    --    end
    --  end

    local x = u[1]

    local loop = append_code(code, u, "loop")
    -- この時点でスタックは空
    append_code(loop, u, "get_local", u.var)
    append_code(loop, u, "get_local", u.var + 1)
    append_code(loop, u, "get_local", u.var + 2)

    append_code(loop, u, "call", 1, #x)
    for i = #x, 1, -1 do
      local v = x[i]
      append_code(loop, u, "set_local", v.var)
    end
    -- この時点でスタックは空

    append_code(loop, u, "get_local", u.var + 4)
    assert(u.var + 4 == x[1].var)
    append_code(loop, u, "push_nil", 1)
    append_code(loop, u, "eq")

    local cond = append_code(loop, u, "if")
    append_code(cond, u, "block")
    append_code(cond, u, "block")
    append_code(cond[1], u, "close", u.var + 3)
    append_code(cond[1], u, "break")
    append_code(cond[2], u, "get_local", u.var + 4)
    append_code(cond[2], u, "set_local", u.var + 2)

    top = process2(scope, u[3], loop, top)
    assert(top == 0)

  elseif u_name == "local_function" then
    append_code(code, u, "closure", u[2].proto.index)
    append_code(code, u, "set_local", u[1].var)

  elseif u_name == "explist" then
    local a = u.adjust
    local v = #u > 0 and u[#u] or nil
    local v_name = v ~= nil and lua54_parser.symbol_names[v[0]] or nil
    if a == nil then
      -- 末尾がfunctioncallまたは...で、かつnomultretが真でなければ、戻り値の個
      -- 数を調節しない。
      if (v_name == "functioncall" or v_name == "...") and not v.nomultret then
        v.nr = -1
        -- TODO これは不要？
        u.nr = #u - 1
      end
    else
      -- #u < a
      --   1. 末尾がfunctioncallまたは...で、かつnomultretが真でなければ、戻り
      --      値の個数を(a-#u+1)個に調節する。
      --   2. 末尾がfunctioncallまたは...で、かつnomultretが真ならば、戻り値の
      --      個数を1個に調節し、(a-#u)個のpush_nil()を追加する。
      --   3. さもなければ、(a-#u)個のpush_nil()を追加する。
      -- #u == a
      --   1. 末尾がfunctioncallまたは...ならば、戻り値の個数を1個に調節する。
      -- #u == a+1
      --   1. 末尾がfunctioncallまたは...ならば、戻り値の個数を0個に調節する。
      --   2. さもなければ、pop(1)を追加する。
      -- #u > a+1
      --   1. 末尾がfunctioncallまたは...ならば、戻り値の個数を0個に調節し、
      --      pop(#u-a-1)を追加する。
      --   2. さもなければ、pop(#u-a)を追加する。
      if v_name == "functioncall" or v_name == "..." then
        if #u < a then
          if not v.nomultret then
            v.nr = a - #u + 1
          else
            v.nr = 1
            u.push = a - #u
          end
        elseif #u == a then
          v.nr = 1
        else
          v.nr = 0
          if #u > a + 1 then
            u.pop = #u - a - 1
          end
        end
      else
        if #u < a then
          u.push = a - #u
        elseif #u > a then
          u.pop = #u - a
        end
      end
    end

  elseif u_name == "..." then
    -- 戻り値の個数が調節されていないfunctioncallと...は、1個に調節する。
    if u.nr == nil then
      u.nr = 1
    end

    append_code(code, u, "vararg", u.nr)
    if u.nr == -1 then
      assert(top >= 0)
      top = u.nr - top
    else
      assert(u.nr > 0)
      top = top + u.nr
    end

  elseif u_name == "functiondef" then
    append_code(code, u, "closure", u[1].proto.index)
    top = top + 1

  elseif u_name == "and" then
    traversed = true

    top = process2(scope, u[1], code, top)

    append_code(code, u, "dup")
    local cond = append_code(code, u, "if")
    top = top - 1
    append_code(cond, u, "block")
    append_code(cond, u, "block")
    append_code(cond[1], u, "pop", 1)

    top = process2(scope, u[2], cond[1], top)

  elseif u_name == "or" then
    traversed = true

    top = process2(scope, u[1], code, top)

    append_code(code, u, "dup")
    local cond = append_code(code, u, "if")
    top = top - 1
    append_code(cond, u, "block")
    append_code(cond, u, "block")
    append_code(cond[2], u, "pop", 1)

    top = process2(scope, u[2], cond[2], top)

  elseif u_name == "." then
    if u.define then
      u.ns_item = 2
      -- top = top + 2
    end

  elseif u_name == ":" then
    traversed = true

    top = process2(scope, u[1], code, top)
    append_code(code, u, "dup")
    top = top + 1
    top = process2(scope, u[2], code, top)

  elseif u_name == "functioncall" then
    -- 戻り値の個数が調節されていないfunctioncallと...は、1個に調節する。
    if u.nr == nil then
      u.nr = 1
    end

    traversed = true

    local x, y = u[1], u[2]
    local x_name = lua54_parser.symbol_names[x[0]]
    if x_name == ":" then
      top = process2(scope, u[1], code, top)
      -- この時点で self self key になっている

      -- self key
      -- self self key

      append_code(code, u, "get_table", 2)
      top = top - 1
      -- この時点で self fun になっている
      --                [top]

      append_code(code, u, "swap")
      -- この時点で fun self になっている
      --               [top]
      local f = top - 1
      assert(f > 0)

      -- 引数を積む
      top = process2(scope, u[2], code, top)

      append_code(code, u, "call", f, u.nr)
      -- assert(top >= 0)
      -- top = top - #y - 2
      top = f - 1
      if u.nr == -1 then
        top = u.nr - top
      else
        top = top + u.nr
      end

    else
      top = process2(scope, u[1], code, top)
      local f = top
      assert(f > 0)
      top = process2(scope, u[2], code, top)

      append_code(code, u, "call", f, u.nr)

      -- assert(top >= 0)
      top = f - 1
      if u.nr == -1 then
        top = u.nr - top
      else
        top = top + u.nr
      end

    end

  elseif u_name == "fieldlist" then
    -- key=value形式でないfieldの個数を数える。
    local x, y
    local ns = 0
    for i, v in ipairs(u) do
      x, y = v[1], v[2]
      v.ns = ns
      if y == nil then
        ns = ns + 1
      end
    end
    u.ns = ns
    -- 末尾がkey=value形式でなく、functioncallまたは...で、かつnomultretが真で
    -- なければ、戻り値の個数を調節しない。
    if x ~= nil and y == nil then
      local x_name = lua54_parser.symbol_names[x[0]]
      if (x_name == "functioncall" or x_name == "...") and not x.nomultret then
        x.nr = -1
        -- TODO これは不要？
        u.nr = ns - 1
      end
    end

    append_code(code, u, "new_table")
    top = top + 1

  elseif u_name == "nil" then
    append_code(code, u, "push_nil", 1)
    top = top + 1

  elseif u_name == "false" then
    append_code(code, u, "push_false")
    top = top + 1

  elseif u_name == "true" then
    append_code(code, u, "push_true")
    top = top + 1

  elseif u_name == "LiteralString" then
    append_code(code, u, "push_literal", u.v)
    top = top + 1

  elseif u_name == "Numeral" then
    append_code(code, u, "push_numeral", u.v, u.hint)
    top = top + 1

  elseif u_name == "Name" then
    -- 1. declareが真ならば、文で命令を生成する。
    -- 2. resolveが真でdefineが真ならば、文で命令を生成する。
    -- 3. resolveが真でdefineが真でなければ、参照命令を生成する。
    -- 4. define_labelまたはresolve_labelが真ならば、文で命令を生成する。
    -- 5. さもなければ、テーブルインデックスとして使用する文字列リテラル命令を
    --    生成する。
    if not u.declare then
      if u.resolve then
        if not u.define then
          if u.var ~= nil then
            if u.var <= 65536 then
              append_code(code, u, "get_local", u.var)
            else
              append_code(code, u, "get_upvalue", u.var - 65536)
            end
            top = top + 1
          else
            assert(u.env ~= nil)
            if u.env <= 65536 then
              append_code(code, u, "get_local", u.env)
            else
              append_code(code, u, "get_upvalue", u.env - 65536)
            end
            append_code(code, u, "push_literal", u.v)
            append_code(code, u, "get_table", 2)
            top = top + 1
          end
        else

          -- set_table/set_fieldを準備
          if u.var == nil then
            assert(u.env ~= nil)
            if u.env <= 65536 then
              append_code(code, u, "get_local", u.env)
            else
              append_code(code, u, "get_upvalue", u.env - 65536)
            end
            append_code(code, u, "push_literal", u.v)
            u.ns_item = 2
            top = top + 2
          end

        end
      elseif not u.define_label and not u.resolve_label then
        append_code(code, u, "push_literal", u.v)
        top = top + 1
      end
    end

  end

  -------------------------------------------------------------------------

  if not traversed then
    for _, v in ipairs(u) do
      top = assert(process2(scope, v, code, top))
    end
  end

  if u.code ~= nil then
    for _, v in ipairs(u.code) do
      append_code(code, u, v[0], v.a, v.b)
      top = top + 1
    end
  end

  -- print(u.v, top)

  -------------------------------------------------------------------------

  if u_name == "=" then
    local x, y = u[1], u[2]
    for i = #x, 1, -1 do
      local v = x[i]
      -- if #v.code > 0 then
      if v.ns_item then
        local j = i + x.ns - v.ns
        append_code(code, u, "set_field", j, j - 1)
        top = top - 1
      else
        assert(v.var ~= nil)
        if v.var <= 65536 then
          append_code(code, u, "set_local", v.var)
        else
          append_code(code, u, "set_upvalue", v.var - 65536)
        end
        top = top - 1
      end
    end
    if x.ns > 0 then
      append_code(code, u, "pop", x.ns)
      top = top - x.ns
    end
    assert(top == 0)

  -- TODO ラベルが解決されていないのであとまわし。
  elseif u_name == "goto" then
    local x = u[1]
    local y = scope.proto.labels:get(x.label).node

    local m = x.locals:size()
    local n = y.locals:size()
    if m <= n then
      for i = 0, n - 1 do
        local var = y.locals:get(n - i)
        if x.locals:get(m - i) ~= var then
          compiler_error("<goto " .. x.v .. "> jumps into the scope of local " .. scope.proto.locals:get(var).name, u)
        end
      end
    end

    for i = 1, m - n do
      local var = x.locals:get(i)
      if scope.proto.locals:get(var).attribute == "close" then
        append_code(code, u, "close", var)
      end
    end
    append_code(code, u, "goto", x.label)

  elseif u_name == "repeat" then
    for j = scope.locals:size(), 1, -1 do
      local var = scope.locals:get(j)
      if scope.proto.locals:get(var).attribute == "close" then
        append_code(code, u, "close", var)
      end
    end

    local cond = append_code(code, u, "if")
    top = top - 1
    assert(top == 0)
    append_code(cond, u, "block")
    append_code(cond, u, "block")
    append_code(cond[1], u, "break")

  elseif u_name == "exp_2or3" then
    if u[3] == nil then
      append_code(code, u, "push_numeral", "1", "DecimalIntegerNumeral")
      top = top + 1
    end

  elseif u_name == "function" then
    -- print(top)
    local v = u[1]
    append_code(code, u, "closure", u[2].proto.index)
    top = top + 1
    if v.ns_item then
      append_code(code, u, "set_table", 3)
      append_code(code, u, "pop", 1)
      top = top - 3
    else
      assert(v.var ~= nil)
      if v.var <= 65536 then
        append_code(code, u, "set_local", v.var)
      else
        append_code(code, u, "set_upvalue", v.var - 65536)
      end
      top = top - 1
    end

    assert(top == 0, top)

  elseif u_name == "local" then
    local x, y = u[1], u[2]
    if y == nil then
      append_code(code, u, "push_nil", #x)
      top = top + #x
    end

    assert(top == #x, u.f)

    for i = #x, 1, -1 do
      local v = x[i]
      if v.attribute == "close" then
        append_code(code, u, "set_local_tbc", v.var)
      else
        append_code(code, u, "set_local", v.var)
      end
      top = top - 1
    end

    assert(top == 0)

  elseif u_name == "return" then
    local v = u[1]

    for _, var in u.locals:ipairs() do
      if scope.proto.locals:get(var).attribute == "close" then
        append_code(code, u, "close", var)
      end
    end

    append_code(code, u, "return")
    if v.nr then
      -- append_code(code, u, "return_nr", v.nr)
      top = -top - v.nr - 1
    else
      -- append_code(code, u, "return", #v)
      top = top - #v
    end
    assert(top == 0, u.f)

  -------------------------------------------------------------------------

  elseif u_name == "varlist" then
    local ns = 0
    for _, v in ipairs(u) do
      v.ns = ns
      -- TODO settableの場合、スタックに2個つまれる。_ENVを参照する場合を考慮し
      -- てコード生成後に調べる。最終的には、スタックの状態を数えるようにする。
      -- if #v.code > 0 then
      if v.ns_item then
        ns = ns + 2
      end
    end
    u.ns = ns

  elseif u_name == "." then
    if not u.define then
      append_code(code, u, "get_table", 2)
      top = top - 1
    end

  elseif u_name == "explist" then
    if u.push ~= nil then
      append_code(code, u, "push_nil", u.push)
      top = top + u.push
    elseif u.pop ~= nil then
      append_code(code, u, "pop", u.pop)
      top = top - u.pop
    end

  -------------------------------------------------------------------------

  elseif u.binop ~= nil then
    append_code(code, u, u.binop)
    top = top - 1
  elseif u.unop ~= nil then
    append_code(code, u, u.unop)

  -------------------------------------------------------------------------

  elseif u_name == "fieldlist" then
    if u.nr ~= nil then
      top = -top - u.nr - 1
      append_code(code, u, "set_list", top)

    elseif u.ns > 0 then
      top = top - u.ns
      append_code(code, u, "set_list", top)
    end

  elseif u_name == "field" then
    if u[2] ~= nil then
      append_code(code, u, "set_table", u.ns + 3)
      top = top - 2
    end

  end

  u.top = top
  -- print(restore_top, save_top)
  if restore_top then
    return save_top
  else
    return top
  end
end

---------------------------------------------------------------------------

-- 仮引数の数
-- 可変長引数を持つか
-- ローカル変数の数（仮引数を含む）
--
-- TODO 暗黙のreturnをどうするか検討する
--   chunkとfuncbodyで違う？

-- TODO スタックの状況を計算する方法を洗練する

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
  process2(scope, chunk)
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

    if proto.labels:empty() then
      out:write "    <labels/>\n"
    else
      out:write "    <labels>\n"
      for j, v in proto.labels:ipairs() do
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
