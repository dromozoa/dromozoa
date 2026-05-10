-- Copyright (C) 2026 Tomoyuki Fujimori <moyu@dromozoa.com>
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
-- along with dromozoa.  If not, see <https://www.gnu.org/licenses/>.

---@diagnostic disable: unused-function, unused-local

---@type fun():integer, boolean
local f = function()
  return 42, true
end

---@type integer, boolean
local x, y

---@class クラス
---@field value integer
local class = {}
local metatable = { __index = class }

---@param value integer
---@return クラス
function class.new(value)
  return setmetatable({ value = value }, metatable)
end

---@alias あ0_-.*い クラス

local x = class.new(42)

---@type あ0_-.*い
local y
y = x

---@alias public あ0_-.*い
---@type public
local x

-- 引数が3個以上で、第1引数がprivate, protected, public, packageのどれかならば、第1引数がscopeを指示する
---@class クラス2: クラス
---@field public public public
local class2 = {}

---@type クラス2
local y
local p = y.public
local v = p.value

---@type (fun(仮*引*数: integer):integer, boolean), string
local f, x = function(p) return p, true end, "foo"

-- @asは型リストに対応していないらしい
local a, b = f(0) --[[@as number, string]]

-- (a + b)という式にアノテーションがつくように見える
local x = a + b --[[@as boolean]]
local y = a + (b --[[@as boolean]])
local z = a --[[@as integer]] + b
local w = a --[[@type integer]]
local v

---@type table<string, integer>
local t
---@type { [string]: integer }
local d
---@type { x: number, y: number, [string]: integer, [integer]: string }
local p
---@type table<string>
local k

---@alias 整数 integer
---@alias 文_.字*-列 string

---@type "r" | "w\t\"abc\\def" | "x"
local mode

---@param x any
---@return any
local function identity(x) return x end

-- @typeは次の行の名前束縛っぽい文に付く
do
  goto Label ---@type integer
  ; ::Label::
  ; identity(mode); --[[foo]] local x; local y
end

local t = {
  ---@type integer
  x = identity(mode),
  ---@type boolean
  [1] = identity(42),
}
local y = t[1]
local z
---@type string
z = identity(true)

--[[
-- アノテーションが付くノードはステートメントに限らない
-- luadoc.lua:2202
local bindDocAccept = {
    'local'     , 'setlocal'  , 'setglobal',
    'setfield'  , 'setmethod' , 'setindex' ,
    'tablefield', 'tableindex', 'self'     ,
    'function'  , 'return'     , '...'      ,
    'call',
}
]]

---@alias 2.3 string
---@alias tuple {[1]:boolean, [2]:string?}
---@type 0|1|2.3|tuple
local x

-- フィールドセパレーター
---@alias table_literal { foo: string; bar: string; }

---@type 0|-1|--comment
local x

---@type 0@comment
local x

---@type 1#comment
local x

---@type boolean?
local a
---@type string?
local b
---@type integer?
local c

-- 優先順位が最低のorよりも結合力が低く見える
local x =  a or  b  --[[@as integer?]]  -- x:  integer?
local y = (a or  b) --[[@as integer?]]  -- y:  integer?
local z =  a or (b  --[[@as integer?]]) -- z: (integer|true)?

-- 論理演算子は短絡があり、関係演算子は結果の型がboolean固定
-- わかりやすいborで観察しても、やはり結合力が低く見える
local x =  a |  b  --[[@as integer?]]  -- x: integer?
local y = (a |  b) --[[@as integer?]]  -- y: integer?
local z =  a | (b  --[[@as integer?]]) -- z: unknown

-- 左結合の場合を考えてみる

local x = a .. b .. (c --[[@as integer?]])
local y = a .. (b .. c --[[@as integer?]])
local z = (a .. b .. c) --[[@as integer?]]

-- 例1: 空文や関数呼び出し文はスキップされて、@classはlocal xにひもづけられる
---@class my.awesome.class
---@field u integer
---@field v string
; print "foo"; local x -- x: my.awesome.class
