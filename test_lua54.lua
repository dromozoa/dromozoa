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
--[[

  basic blockを作ることを考えたら、if else endはあってもいい？

  定数は
    string literal
    numeral
  の2種類で、numeralは解析するとint64_tかdoubleかわかる。

  push_string const_string("...")
  push_number const_number("...", hint) -- d f x a
  push_nil()
  push_false(v)
  push_true(v)

  nil;
  false;
  true;
  i64.const "..." [hex|dec];
  f64.const "..." [hex|dec];
  string.const "...";

  {
    foo=1;
    2;
    bar=3;
    4;
    baz=5;
  }

  newtable(hint_nkeys, hint_nitems)
  $t=$top
  push(1)
  push("foo")
  settable($t)
  push(2)
  push(3)
  push("bar")
  settable($t)
  push(4)
  push(5)
  push("baz")
  settable($t)
  setlist($t)

]]
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

local function resolve(scope, name)
  local proto = scope.proto
  repeat
    for i = scope.locals:size(), 1, -1 do
      local var = scope.locals:get(i)
      if proto.locals:get(var).name == name then
        return var
      end
    end
    scope = scope.parent
    if scope == nil then
      return
    end
  until proto ~= scope.proto

  local var = resolve(scope, name)
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

local function def_label(scope, name, u)
  local label, v = find_label(scope, name)
  if label ~= nil then
    compiler_error("label " .. name .. " already defined on line " .. v.node.n, u)
  end
  local label = scope.proto.labels:append{name=name, node=u}:size()
  scope.labels:append(label)
  return label
end

local function ref_label(scope, name, u)
  local label = find_label(scope, name)
  if label == nil then
    compiler_error("no visible label " .. name, u)
  end
  return label
end

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

local function process1(protos, proto, scope, u)
  if u.proto ~= nil then
    u.proto.labels = array()
    u.proto.locals = array()
    u.proto.upvalues = array()
    u.proto.parent = proto
    proto = u.proto
    protos:append(proto)
  end

  if u.scope ~= nil then
    u.scope.labels = array()
    u.scope.locals = array()
    u.scope.proto = proto
    u.scope.parent = scope
    scope = u.scope
  end

  local u_name = lua54_parser.symbol_names[u[0]]

  if u_name == "funcbody" then
    assert(u.proto == proto)
    assert(u.scope == scope)
    -- colon syntaxで関数が定義されたら、暗黙の仮引数selfを追加する。
    if proto.self then
      u.var = declare(scope, "self", u)
    end
  elseif u_name == "for" then
    assert(u.scope == scope)
    -- 内部的に3個の変数を使用する。
    u.var = declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
  elseif u_name == "for_in" then
    -- 内部的に4個の変数を使用する。Lua 5.3以前は3個だったが、Lua 5.4で
    -- to-be-closed変数が追加された。
    assert(u.scope == scope)
    u.var = declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u, "close")
  end

  if u.declare then
    u.var = declare(scope, u.v, u, u.attribute)
  elseif u.resolve then
    local var = resolve(scope, u.v)
    if var == nil then
      u.env = resolve(scope, "_ENV")
    else
      u.var = var
    end
  elseif u.def_label then
    u.label = def_label(scope, u.v, u)
  end

  if u_name == "explist" then
    -- explistの末尾であれば、multretになりえる。
    -- for i = 1, #u - 1 do
    --   u[i].multret = nil
    -- end

    local a = u.adjust
    local v = #u > 0 and u[#u] or nil
    local v_name = v ~= nil and lua54_parser.symbol_names[v[0]] or nil
    if a == nil then
      if v_name == "functioncall" or v_name == "..." then
        if v ~= nil and not v.nomultret then
          v.nr = -1
          u.nr = #u - 1
        end
      end

      -- if v ~= nil and v.multret then
      --   u.nr = #u - 1
      -- end
    else
      -- #u < a
      --   1. 末尾がfunctioncallまたは...で、nomultretが真でなければ、戻り値の
      --      個数を(a-#u+1)個に設定する。
      --   2. さもなければ、(a-#u)個のpush_nil()を追加する。
      -- #u == a
      --   1. 末尾がfunctioncallまたは...ならば、戻り値の個数を1個に設定する。
      -- #u == a+1
      --   1. 末尾がfunctioncallまたは...ならば、戻り値の個数を0個に設定する。
      --   2. さもなければ、pop(1)を追加する。
      -- #u > a+1
      --   1. 末尾がfunctioncallまたは...ならば、戻り値の個数を0個に設定し、
      --      pop(#u-a-1)を追加する。
      --   2. さもなければ、pop(#u-a)を追加する。
      if #u < a then
        if v_name == "functioncall" or v_name == "..." then
          if v ~= nil and not v.nomultret then
            v.nr = a - #u + 1
          else
            v.nr = 1
            u.push = a - #u
          end
        else
          u.push = a - #u
        end

        -- if v ~= nil and not v.nomultret then
        --   v.nr = a - #u + 1
        -- else
        --   u.push = a - #u
        -- end
      else
        -- local v_name = lua54_parser.symbol_names[v[0]]
        if v_name == "functioncall" or v_name == "..." then
          if #u == a then
            v.nr = 1
          else
            v.nr = 0
            if #u > a + 1 then
              u.pop = #u - a - 1
            end
          end
        elseif #u > a then
          u.pop = #u - a
        end
      end
    end

  elseif u_name == "fieldlist" then
    -- 1. key=value形式でないfieldの個数を数える。
    -- 2. fieldlistの末尾であり、かつkey=value形式でなければ、multretになりえる。
    local nlist = 0
    for i, v in ipairs(u) do
      if v[2] == nil then
        -- key=value形式でない
        nlist = nlist + 1
        -- if i < #u then
        --   v[1].multret = nil
        -- end
      else
        -- key=value形式である
        -- v[1].multret = nil
      end
      v.nlist = nlist
    end
    u.nlist = nlist

    local v = #u > 0 and u[#u] or nil
    local v_name = v ~= nil and lua54_parser.symbol_names[v[1][0]] or nil
    if v_name == "functioncall" or v_name == "..." then
      if not v[1].nomultret and v[2] == nil then
        v[1].nr = -1
        u.nr = nlist - 1
      end
    end
  end

  if u_name == "functioncall" or u_name == "..." then
    if u.nr == nil then
      u.nr = 1
    end
  end

  for _, v in ipairs(u) do
    process1(protos, proto, scope, v)
  end

  local code = {}
  if u.code ~= nil then
    for _, v in ipairs(u.code) do
      append_code(code, u, v[0], v.a, v.b)
    end
  elseif u.binop ~= nil then
    append_code_unpack(code, u[1].code)
    append_code_unpack(code, u[2].code)
    append_code(code, u, u.binop)
  elseif u_name == "and" then
    append_code_unpack(code, u[1].code)
    append_code(code, u, "dup")
    local conditional = append_code(code, u, "if")
    local then_block = append_code(conditional, u, "then_block")
    local else_block = append_code(conditional, u, "else_block")
    append_code(then_block, u, "pop")
    append_code_unpack(then_block, u[2].code)
  elseif u_name == "or" then
    append_code_unpack(code, u[1].code)
    append_code(code, u, "dup")
    local conditional = append_code(code, u, "if")
    local then_block = append_code(conditional, u, "then_block")
    local else_block = append_code(conditional, u, "else_block")
    append_code(else_block, u, "pop")
    append_code_unpack(else_block, u[2].code)
  elseif u.unop ~= nil then
    append_code_unpack(code, u[1].code)
    append_code(code, u, u.unop)
  elseif u_name == "..." then
    append_code(code, u, "vararg", u.nr)
  elseif u_name == "fieldlist" then
    append_code(code, u, "newtable")
    for _, v in ipairs(u) do
      append_code_unpack(code, v.code)
    end
    if u.nr ~= nil then
      append_code(code, u, "setlist_nr", u.nr)
    elseif u.nlist > 0 then
      append_code(code, u, "setlist", u.nlist)
    end
  elseif u_name == "field" then
    if u[2] == nil then
      append_code_unpack(code, u[1].code)
    else
      append_code_unpack(code, u[1].code)
      append_code_unpack(code, u[2].code)
      append_code(code, u, "settable", u.nlist + 3)
    end
  end
  u.code = code
end

local function process2(scope, u)
  if u.scope ~= nil then
    scope = u.scope
  end

  if u.ref_label then
    u.label = ref_label(scope, u.v, u)
  end

  for _, v in ipairs(u) do
    process2(scope, v)
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
  "declare", "resolve", "var", "env";
  "def_label", "ref_label", "label";
  "adjust", "multret", "nr", "push", "pop", "nlist";
  "hint";
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
    out:write("  <proto index=\"", i, "\"")
    dump_attrs(out, proto, {"self", "vararg"})
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
        out:write("       <upvalue index=\"", j, "\"")
        dump_attrs(out, v, {"name", "var"})
        out:write "/>\n"
      end
      out:write "    </upvalues>\n"
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
