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

local function declare(scope, name, u)
  local var = scope.proto.locals:append{name=name, node=u}:size()
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
      declare(scope, "self", u)
    end
  elseif u_name == "for" then
    assert(u.scope == scope)
    -- 内部的に3個の変数を使用する。
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
  elseif u_name == "for_in" then
    -- 内部的に4個の変数を使用する。Lua 5.3以前は3個だったが、Lua 5.4で
    -- to-be-closed変数が追加された。
    assert(u.scope == scope)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
  end

  if u.declare then
    u.var = declare(scope, u.v, u)
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

  -- adjustを解決する
  -- expは、...とfunctioncall以外はadjust=1である。
  -- 簡単のため、これをadjust=nilとする
  -- ...とfunctioncallは、MULTRET=-1で初期化する

  -- explistとfieldlistの末尾の...とfunctioncallはMULTIRETになりうる。
  -- explistの調整数は左辺のvarlistやnamelistによって定まる

  for _, v in ipairs(u) do
    process1(protos, proto, scope, v)
  end
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
  "adjust";
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

  if #u == 0 and (code == nil or #code == 0) then
    out:write "/>\n"
  else
    out:write ">\n"

    for _, v in ipairs(u) do
      dump_node(out, v, n)
    end

    if code ~= nil then
      for i, v in ipairs(code) do
        out:write(("  "):rep(n + 1), "<code op=", quote(v[0]))
        for i, a in ipairs(v) do
          out:write(" a", i, "=", quote(a))
        end
        out:write "/>\n"
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
        dump_attrs(out, v, {"name"})
        if v.node ~= nil then
          dump_attrs(out, v.node, {"attribute", "n", "c"})
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
