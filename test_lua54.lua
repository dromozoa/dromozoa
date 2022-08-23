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

local function check_jump(u, v)
  local m = u.locals:size()
  local n = v.locals:size()

  -- 7 6 5 4 3 2 1 m=7
  --         3 2 1 n=3
  --         3 2 1

  -- 7 6 5 4 3 2 1 m=7
  --       8 7 2 1 n=4
  --           2 1

  --         3 2 1 m=3
  --         8 2 1 n=3
  --           2 1

  if not v.end_of_scope then
    if m < n then
      return false
    end
    for i = 1, n do
      if u.locals:get(m - n + i) ~= v.locals:get(i) then
        return false
      end
    end
  end

  for i = 0, n do
    if u.locals:get(m - i) ~= v.locals:get(n - i) then
      return i
    end
  end
  assert(m == n)
  return m
end

---------------------------------------------------------------------------

local function code(u, op, a, b, c)
  return { [0] = op, a = a, b = b, c = c, node = u }
end

local function append_code(self, u, op, a, b)
  local v = { [0] = op, a = a, b = b, c = c, node = u }
  self[#self + 1] = v
  return v
end

local function append_code_unpack(self, that)
  for _, v in ipairs(that) do
    self[#self + 1] = v
  end
end

---------------------------------------------------------------------------

local function process1(protos, proto, scope, u, loop)
  if u.proto ~= nil then
    u.proto.labels = array()
    u.proto.locals = array()
    u.proto.upvalues = array()
    u.proto.scopes = array()
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
      -- empty statementsは解析の時点でとりのぞかれるので、ラベル文だけがvoid
      -- statementsである。スコープは、スコープの最後のvoid statementsの前で終
      -- 了するとみなす。repeat-until文以外のスコープで、ブロックの末尾にラベル
      -- 文があるかどうかを検査する。
      if not scope.repeat_until then
        for i = #u, 1, -1 do
          local v = u[i]
          if lua54_parser.symbol_names[v[0]] == "label" then
            v[1].end_of_scope = true
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
      u.locals = collect(scope)
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

local function process2(scope, u)
  if u.scope ~= nil then
    scope = u.scope
  end

  if u.resolve_label then
    u.label = resolve_label(scope, u.v, u)
  end

  local u_name = lua54_parser.symbol_names[u[0]]

  for _, v in ipairs(u) do
    process2(scope, v)
  end

  if u_name == "break" then
  elseif u_name == "goto" then
    local x = u[1]
    local y = assert(scope.proto.labels:get(x.label).node)
    local result = check_jump(x, y)

    if not result then
      compiler_error("<goto " .. x.v .. "> jumps into the scope of local " .. scope.proto.locals:get(y.locals:get(1)).name, u)
    end

    -- goto文とlabel文にcloseをはりつける
    -- scopeのcloseを生成するときは、label文をチェックする
    -- これだと、複数のラベル文をひとつにまとめる必要がある。

    print("x.locals", x.locals:concat",")
    print("y.locals", y.locals:concat",")

    print("goto", x.v)
    for i = 1, x.locals:size() - result do
      local var = x.locals:get(i)
      if scope.proto.locals:get(var).attribute == "close" then
        print("close x", var)
      end
    end
    for i = 1, y.locals:size() - result do
      local var = y.locals:get(i)
      if scope.proto.locals:get(var).attribute == "close" then
        print("close y", var)
      end
    end
  end
-- Lua:    label.lua:47: <goto L4> at line 44 jumps into the scope of local 'a'
-- LuaJIT: label.lua:44: <goto L4> jumps into the scope of local 'a'

end

---------------------------------------------------------------------------

local function process2_(scope, u)
  if u.scope ~= nil then
    scope = u.scope
  end

  if u.resolve_label then
    u.label = resolve_label(scope, u.v, u)
  end

  local u_name = lua54_parser.symbol_names[u[0]]

  if u_name == "explist" then
    local a = u.adjust
    local v = #u > 0 and u[#u] or nil
    local v_name = v ~= nil and lua54_parser.symbol_names[v[0]] or nil
    if a == nil then
      -- 末尾がfunctioncallまたは...で、かつnomultretが真でなければ、戻り値の個
      -- 数を調節しない。
      if (v_name == "functioncall" or v_name == "...") and not v.nomultret then
        v.nr = -1
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
        u.nr = ns - 1
      end
    end
  elseif u_name == "functioncall" or u_name == "..." then
    -- 戻り値の個数が調節されていないfunctioncallと...は、1個に調節する。
    if u.nr == nil then
      u.nr = 1
    end
  end

  for _, v in ipairs(u) do
    process2(scope, v)
  end

  if u.code ~= nil then
    for _, v in ipairs(u.code) do
      v.node = u
    end
    return
  end

  u.code = {}
  if u_name == "..." then
    append_code(u.code, u, "vararg", u.nr)
  elseif u_name == "functiondef" then
    append_code(u.code, u, "closure", u[1].proto.index)
  elseif u.binop ~= nil then
    append_code_unpack(u.code, u[1].code)
    append_code_unpack(u.code, u[2].code)
    append_code(u.code, u, u.binop)
  elseif u_name == "and" then
    append_code_unpack(u.code, u[1].code)
    append_code(u.code, u, "dup", 1)
    local cond = append_code(u.code, u, "if")
    append_code(cond, u, "block")
    append_code(cond, u, "block")
    append_code(cond[1], u, "pop", 1)
    append_code_unpack(cond[1], u[2].code)
  elseif u_name == "or" then
    append_code_unpack(u.code, u[1].code)
    append_code(u.code, u, "dup", 1)
    local cond = append_code(u.code, u, "if")
    append_code(cond, u, "block")
    append_code(cond, u, "block")
    append_code(cond[2], u, "pop", 1)
    append_code_unpack(cond[2], u[2].code)
  elseif u.unop ~= nil then
    append_code_unpack(u.code, u[1].code)
    append_code(u.code, u, u.unop)

  elseif u_name == "fieldlist" then
    append_code(u.code, u, "new_table")
    for _, v in ipairs(u) do
      append_code_unpack(u.code, v.code)
    end
    if u.nr ~= nil then
      append_code(u.code, u, "set_list_nr", u.nr)
    elseif u.ns > 0 then
      append_code(u.code, u, "set_list", u.ns)
    end
  elseif u_name == "field" then
    if u[2] == nil then
      append_code_unpack(u.code, u[1].code)
    else
      append_code_unpack(u.code, u[1].code)
      append_code_unpack(u.code, u[2].code)
      append_code(u.code, u, "set_table", u.ns + 3)
    end
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
              append_code(u.code, u, "get_local", u.var)
            else
              append_code(u.code, u, "get_upvalue", u.var - 65536)
            end
          else
            assert(u.env ~= nil)
            if u.env <= 65536 then
              append_code(u.code, u, "get_local", u.env)
            else
              append_code(u.code, u, "get_upvalue", u.env - 65536)
            end
            append_code(u.code, u, "push_literal", u.v)
            append_code(u.code, u, "get_table", 2)
          end
        else

          -- set_fieldを準備
          if u.var == nil then
            assert(u.env ~= nil)
            if u.env <= 65536 then
              append_code(u.code, u, "get_local", u.env)
            else
              append_code(u.code, u, "get_upvalue", u.env - 65536)
            end
            append_code(u.code, u, "push_literal", u.v)
          end

        end
      elseif not u.define_label and not u.resolve_label then
        append_code(u.code, u, "push_literal", u.v)
      end
    end
  elseif u_name == "." then
    if not u.define then
      append_code_unpack(u.code, u[1].code)
      append_code_unpack(u.code, u[2].code)
      append_code(u.code, u, "get_table", 2)
    else
      -- set_fieldを準備する
      append_code_unpack(u.code, u[1].code)
      append_code_unpack(u.code, u[2].code)
    end
  elseif u_name == "explist" then
    for _, v in ipairs(u) do
      append_code_unpack(u.code, v.code)
    end
    if u.push ~= nil then
      append_code(u.code, u, "push_nil", u.push)
    elseif u.pop ~= nil then
      append_code(u.code, u, "pop", u.pop)
    end
  elseif u_name == "functioncall" then
    local x, y = u[1], u[2]
    local x_name = lua54_parser.symbol_names[x[0]]
    if x_name == ":" then
      append_code_unpack(u.code, x[1].code)
      append_code(u.code, u, "dup", 1)
      append_code_unpack(u.code, x[2].code)
      append_code(u.code, u, "get_table", 2)
      append_code(u.code, u, "swap", 2)
      append_code_unpack(u.code, y.code)
      if y.nr ~= nil then
        append_code(u.code, u, "call_nr", y.nr + 1, u.nr)
      else
        append_code(u.code, u, "call", #y + 1, u.nr)
      end
    else
      append_code_unpack(u.code, x.code)
      append_code_unpack(u.code, y.code)
      if y.nr ~= nil then
        append_code(u.code, u, "call_nr", y.nr, u.nr)
      else
        append_code(u.code, u, "call", #y, u.nr)
      end
    end
  elseif u_name == "varlist" then
    local ns = 0
    for _, v in ipairs(u) do
      v.ns = ns
      if #v.code > 0 then
        ns = ns + 2
        append_code_unpack(u.code, v.code)
      end
    end
    u.ns = ns
  elseif u_name == "=" then
    local x, y = u[1], u[2]
    append_code_unpack(u.code, x.code)
    append_code_unpack(u.code, y.code)
    for i = #x, 1, -1 do
      local v = x[i]
      if #v.code > 0 then
        local j = i + x.ns - v.ns
        append_code(u.code, u, "set_field", j, j - 1)
      else
        assert(v.var ~= nil)
        if v.var <= 65536 then
          append_code(u.code, u, "set_local", v.var)
        else
          append_code(u.code, u, "set_upvalue", v.var - 65536)
        end
      end
    end
    if x.ns > 0 then
      append_code(u.code, u, "pop", x.ns)
    end
  elseif u_name == "label" then
    append_code(u.code, u, "label", u[1].label)
  elseif u_name == "goto" then
    append_code(u.code, u, "goto", u[1].label)
  elseif u_name == "block" then
    local block = append_code(u.code, u, "block")
    for _, v in ipairs(u) do
      append_code_unpack(block, v.code)
    end
  elseif u_name == "do" then
    append_code_unpack(u.code, u[1].code)

  elseif u_name == "while" then
    local block = append_code(u.code, u, "block")
    append_code_unpack(block, u[1].code)
    local cond = append_code(block, u, "if")
    append_code(cond, u, "block")
    append_code(cond, u, "block")
    local loop = append_code(cond[1], u, "loop")
    append_code_unpack(loop, u[2].code)
    append_code_unpack(loop, u[1].code)
    local cond = append_code(loop, u, "if")
    append_code(cond, u, "block")
    append_code(cond, u, "block")
    append_code(cond[2], u, "break")

  elseif u_name == "repeat" then
    local loop = append_code(u.code, u, "loop")
    append_code_unpack(loop, u[1].code)
    append_code_unpack(loop, u[2].code)
    local cond = append_code(loop, u, "if")
    append_code(cond, u, "block")
    append_code(cond, u, "block")
    append_code(cond[1], u, "break")

  elseif u_name == "local" then
    local x, y = u[1], u[2]
    if y == nil then
      append_code(u.code, u, "push_nil", #x)
    else
      append_code_unpack(u.code, y.code)
    end
    for i = #x, 1, -1 do
      local v = x[i]
      append_code(u.code, u, "set_local", v.var)
    end
  end
end

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
