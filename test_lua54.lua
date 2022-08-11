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

--[[

  scope--
    |    \
  scope-- \
    |    \ \
  scope-- \ \
    |    \ \ \
  scope----proto
    |        |
  scope--    |
    |    \   |
  scope-- \  |
    |    \ \ |
  scope -- proto
    |        |
  scope--    |
    |    \   |
  scope-- \  |
    |    \ \ |
  scope -- proto <= chunk

  scopeの役割
  1. name検索  : nameから変数インデックスへの変換
  2. tbcの管理 : scopeに含まれる変数インデックスの範囲

  name名前探索
    nameに合致する変数インデックスが見つかるまでscopeをたどる。見つかったら、
      (index, proto)
    の組が得られる。

  name名前解決
    nameに合致する変数インデックスが見つかるまでscopeをたどる。見つかったら、
      (index, proto)
    の組が得られる。帰りがけに上位値の登録を行いながら戻る
      (index, proto_n), ..., proto_1, proto_0
    protoは上位値の表を持つ。

  変数と上位値はそれぞれ256個までとする。
]]

--[====[
local function find_scope(u)
  while u.scope == nil do
    u = u.parent
  end
  return u.scope
end

-- local function find_proto(u)
--   while u.proto == nil do
--     u = u.parent
--   end
--   return u.proto
-- end

local function declare(u, name)
  if name == nil then
    assert(_[u[0]] == "Name")
    name = u.v
  end

  local scope = find_scope(u)
  local proto = scope.proto

  local n = #proto.locals + 1
  proto.locals[n] = name
  scope.locals[#scope.locals + 1] = n
end

local function search(u)
  local scope = find_scope(u)
  local proto = scope.proto

  while true do
    for i = #scope.locals, 1, -1 do
      local name = proto.locals[scope.locals[i]]
      if name == u.v then
        return "v", scope.locals[i]
      end
    end

    if proto ~= scope.parent.proto then
      for i = #proto.upvalues, 1, -1 do
        local v = proto.upvalues[i]
        if v[1] == u.v then
          return "u", i
        end
      end
      break
    end

    scope = scope.parent
  end
end

local function resolve(u, proto, scope)
  local name = _[u[0]]

  if u.proto ~= nil then
    u.proto.parent = proto
    proto = u.proto

    assert(u.scope ~= nil)
    u.scope.parent = scope
    u.scope.proto = proto
    scope = u.scope

    if name == "chunk" then
      assert(not proto.self)
      proto.max_param = 0
    else
      assert(name == "funcbody")
      if proto.self then
        declare(u, "self")
        proto.max_param = #u[1] + 1
      else
        proto.max_param = #u[1]
      end
    end
  elseif u.scope ~= nil then
    u.scope.parent = scope
    u.scope.proto = proto
    scope = u.scope
  end

  if name == "for" then
    declare(u, "(for)")
    declare(u, "(for)")
    declare(u, "(for)")
  elseif name == "for_in" then
    declare(u, "(for)")
    declare(u, "(for)")
    declare(u, "(for)")
    declare(u, "(for)")
  end

  if u.declare then
    declare(u)
  end

  if u.resolve then
    resolve_name(u)
  end

  for i = 1, #u do
    local v = u[i]
    v.parent = u
    resolve(v, proto, scope)
  end
end

local function process(chunk)
  -- BLOCKはスコープを生成する。以下はBLOCKの外側にスコープがはみだすとみなす。
  -- 1. for文
  -- 2. 関数本体
  -- 3. repeat文

  -- protoは木構造を持つ。
  -- scopeは木構造を持つ。

  -- 局所変数を生成する場所
  --   numerical for
  --     内部的に3個の局所変数が追加される
  --   generic for
  --     内部的に4個の局所変数が追加される（Lua 5.4でto-be-closedが追加された）
  --   local function
  --   local
  --   関数本体の仮引数
  --
  -- ラベルはlabel文によって生成され、goto文によって参照される。局所変数と異な
  -- り、名前の前方参照が可能だが、名前の上書きができない。

  -- 処理の進行順序と木構造の順序を合わせる
  -- 1. "="と"in"の左辺と右辺の順序を逆にする
  -- 2. 代入文とフィールドも入れ替える。
  -- 3. local function文は入れ替えない。

  -- プロトタイプごとに番号をつけていく
  -- 変数   (local variable) v1...
  -- 上位値 (upvalue)  u1...
  -- name = { v1 or u1, name, NameToken, { def, use, updef, upuse } }
  --
  -- Lua 5.4
  --   変数   上限は200個
  --   上位値 上限は255個
  --
  --
  --
  --
  --[[
#if LUAI_IS32INT
#define LUAI_MAXSTACK		1000000
#else
#define LUAI_MAXSTACK		15000
#endif

#define LUA_REGISTRYINDEX	(-LUAI_MAXSTACK - 1000)
#define lua_upvalueindex(i)	(LUA_REGISTRYINDEX - (i))

    局所変数を宣言すると、もっとも近いスコープに名前を作成する。
    プロトタイプに変数として追加する。

    変数を参照しようとすると、スコープを上にたどっていって名前を調べる。
    同一プロトタイプのなかで見つかれば、局所変数の参照となる。
    プロトタイプに登録されている上位値で見つかれば、上位値の参照となる。
    プロトタイプ外で見つかった場合、上位値の登録をしたうえで、上位値の参照となる。
    見つからなかった場合、_ENVへのSETTABLE/GETTABLEの処理になる

    find_name
      => v    variable
              same proto, outer proto
      => u    upvalue
      => nil  not found

    ref_name
    def_name

    p1 {
      s1->s2->s3;
      locals = {
        "foo",
        "bar",
        "baz",
      }
      upvalues = {}
    }

    s1 = {
      locals = { "foo" }
    }
    s2 = {
      locals = { "bar" }
    }
    s3 = {
      locals = { "baz" }
    }

    p2 {
      locals = {
        { "qux" V1 sym },
      }
      upvalues = {
        { name, proto.V3 }
        "baz" U1=>proto.V3,
        "bar" U2=>proto.V2,
        "foo" U3=>proto.V1,
      }
    }
    s4 = {
      locals = {
        1, -- "qux" V1,
      }
    }

    function f1()
      local foo = 1
      do
        local bar = 2
        do
          local baz = 3
          function f2()
            local qux = 4
            print(baz) U1
            print(bar) U2
            print(foo) U3
            print(qux) V1
          end
        end
      end
    end

    V1..V256
    U1..U256 (257..512)




  ]]

  local proto = {
--[[
    parent = parent;
    locals = {
      { "_ENV", NameToken }
    };
    labels = {};
    upvalues = {
      { "name", "U", index };
      { "name", "V", index };
    };
]]
  }

  local scope = {
--[[
    parent = parent;
    proto = proto;
    locals = {1};
    labels = {};
]]
  }

  resolve(chunk, proto, scope)

  -- resolve_names(chunk[1], chunk)
end
]====]

---------------------------------------------------------------------------

--[[

  scope--
    |    \
  scope-- \
    |    \ \
  scope-- \ \
    |    \ \ \
  scope----proto <= funcbody
    |        |
  scope--    |
    |    \   |
  scope-- \  |
    |    \ \ |
  scope----proto <= funcbody
    |        |
  scope--    |
    |    \   |
  scope-- \  |
    |    \ \ |
  scope----proto <= chunk
    |        |
  scope----proto <= external

]]

local function declare(scope, name, u)
  scope.locals:append(scope.proto.locals:append{name=name, node=u}:size())
end

local function resolve(proto, scope, name)
end

local function process1(u, proto, scope, parent)
  if u.proto ~= nil then
    u.proto.parent = proto
    u.proto.locals = array()
    u.proto.upvalues = array()
    proto = u.proto
  end

  if u.scope ~= nil then
    u.scope.proto = proto
    u.scope.parent = scope
    u.scope.locals = array()
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
    -- 内部的に4個の変数を使用する。
    -- Lua 5.3以前は3個だったが、Lua 5.4でto-be-closed変数が追加された。
    assert(u.scope == scope)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
    declare(scope, "(for state)", u)
  end

  if u.declare then
    declare(scope, u.v, u)
  end

  if u.resolve then
    resolve(scope, proto, u.v)
  end

  for _, v in ipairs(u) do
    process1(v, proto, scope, u)
  end
end

local function process(chunk)
  -- chunkの外側に_ENVを用意する。
  local proto = { locals = array() }
  local scope = { locals = array() }
  scope.locals:append(proto.locals:append { name = "_ENV" }:size())
  process1(chunk, proto, scope)
end

---------------------------------------------------------------------------

local function quote(s)
  return '"' .. string.gsub(s, '[&<>"]', { ['&'] = '&amp;', ['<'] = '&lt;', ['>'] = '&gt;', ['"'] = '&quot;' }) .. '"'
end

local attrs = { "v", "scope", "self", "vararg", "attribute", "declare", "resolve", "type" }
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
