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
--
-- https://github.com/aidansteele/osx-abi-macho-file-format-reference
-- https://developers.wonderpla.net/entry/2021/03/19/105503

--[[
  正規表現 (DFA) レキサ

  正規表現でないレキサは後で実装する
  レキサの生成自体はFull Luaで実装してよい
  生成されたコードはTiny Luaで実装する
]]

-- set "abc" -- [abc]
-- range "az" -- [a-z]

local regexp = require "dromozoa.regexp"
local dump_ast = require "dromozoa.regexp.dump_ast"

local P = regexp.pattern.pattern
local R = regexp.pattern.range
local S = regexp.pattern.set

dump_ast(io.stdout, P(1) * P"abc" * (R"09" + S"abc"))


