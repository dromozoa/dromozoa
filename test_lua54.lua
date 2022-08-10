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

local _ = lua54_parser.symbol_names

local function declare_name()
end

local function resolve_names(u)
  local name = _[u[0]]

  if name == "for" then
    print("for", "(1,2,3)")
    print("for", u[2].v)
  elseif name == "for_in" then
    print("for_in", "(1,2,3,4)")
    for _, v in ipairs(u[2]) do
      print("for_in", v.v)
    end
  elseif name == "local_function" then
    print("local_function", u[1].v)
  elseif name == "local" then
    for _, v in ipairs(u[2]) do
      print("local", v.v)
    end
  elseif name == "funcbody" then
    if u.self then
      print("funcbody", "self")
    end
    for _, v in ipairs(u[1]) do
      print("funcbody", v.v)
    end
    if u.vararg then
      print("funcbody", "...")
    end
  end

  for i = 1, #u do
    resolve_names(u[i], u)
  end
end

--[[

  変数の参照は、u_i, v_i, L_i で行う

  proto = {
    self = true;
    vararg = true;
    -- 仮引数の個数
    max_param = 4;

    locals = {
      { "name" [, NameToken] };
    };
    upvalues = {
      { "name", "[uv]", index }; -- 親protoのlocalsかupvaluesを参照
    };
    labels = {
      { "name" [, NameToken] };
    };
  }

  scope = {
    parent = scope;
    proto = proto;
    locals = { index };
    labels = { index };
  }





]]

local function append(list, value)
  list[#list + 1] = value
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
      local proto_locals = proto.locals
      local scope_locals = scope.locals

      assert(#proto_locals == 0)
      assert(#scope_locals == 0)

      local n = 0
      if proto.self then
        n = n + 1
        proto_locals[n] = "self"
        scope_locals[n] = n
      end
      for _, v in ipairs(u[1]) do
        n = n + 1
        proto_locals[n] = v.v
        scope_locals[n] = n
      end
      proto.max_param = n
    end
  elseif u.scope ~= nil then
    u.scope.parent = scope
    u.scope.proto = proto
    scope = u.scope
  end




  for i = 1, #u do
    resolve(u[i], proto, scope)
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

---------------------------------------------------------------------------

local function quote(s)
  return '"' .. string.gsub(s, '[&<>"]', { ['&'] = '&amp;', ['<'] = '&lt;', ['>'] = '&gt;', ['"'] = '&quot;' }) .. '"'
end

local attrs = { "v", "attribute", "scope", "self", "vararg", "type" }
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
