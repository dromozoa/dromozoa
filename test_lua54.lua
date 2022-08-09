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

local function process(node)
  -- BLOCKはスコープを作成する。for文と関数本体はBLOCKの外側がスコープに含まれる。
  --
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


end

---------------------------------------------------------------------------

local function quote(s)
  return '"' .. string.gsub(s, '[&<>"]', { ['&'] = '&amp;', ['<'] = '&lt;', ['>'] = '&gt;', ['"'] = '&quot;' }) .. '"'
end

local attrs = { "v" }
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
    if u[attr] ~= nil then
      out:write(" ", attr, "=", quote(u[attr]))
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

  local out = assert(io.open(result_basename .. "_tree.xml", "w"))
  dump(out, root)
  out:close()
end
