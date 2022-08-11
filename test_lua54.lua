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

  for i, u in proto.upvalues:ipairs() do
    if u.index == var then
      assert(u.name == name)
      return i + 65536
    end
  end
  return proto.upvalues:append{name=name, index=var}:size() + 65536
end

local function process1(u, proto, scope, parent)
  if u.proto ~= nil then
    u.proto.locals = array()
    u.proto.upvalues = array()
    u.proto.parent = proto
    proto = u.proto
  end

  if u.scope ~= nil then
    u.scope.locals = array()
    u.scope.proto = proto
    u.scope.parent = scope
    scope = u.scope
  end

  local u_name = lua54_parser.symbol_names[u[0]]

  -- 暗黙の変数宣言
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
  end

  for _, v in ipairs(u) do
    process1(v, proto, scope, u)
  end
end

local function process(chunk)
  --
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
  --
  local external_proto = { locals = array() }
  local external_scope = { locals = array(), proto = external_proto }
  declare(external_scope, "_ENV")
  process1(chunk, external_proto, external_scope)
end

---------------------------------------------------------------------------

local function quote(s)
  return '"' .. string.gsub(s, '[&<>"]', { ['&'] = '&amp;', ['<'] = '&lt;', ['>'] = '&gt;', ['"'] = '&quot;' }) .. '"'
end

local attrs = { "v", "attribute", "declare", "resolve", "var", "env", "type" }
if verbose then
  for _, attr in ipairs { "i", "j", "f", "n", "c", "s" } do
    attrs[#attrs + 1] = attr
  end
end

local function dump(out, u, n)
  if n == nil then
    n = 0
  else
    n = n + 1
  end

  out:write(("  "):rep(n), "<node")
  if u[0] ~= nil then out:write(" name=", quote(lua54_parser.symbol_names[u[0]])) end
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

  if #u == 0 then
    out:write "/>\n"
  else
    out:write ">\n"
    for _, v in ipairs(u) do
      dump(out, v, n)
    end
    out:write(("  "):rep(n), "</node>\n")
  end
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
    dump(out, token)
    return parse(token)
  end)

  out:write "</nodes>\n"
  out:close()

  process(root)

  local out = assert(io.open(result_basename .. "_tree.xml", "w"))
  dump(out, root)
  out:close()
end
