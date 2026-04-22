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

--BEGIN--

local expect = {
  3,
  345,
  0xff,
  0xBEBADA,
  3.0,
  3.1416,
  314.16e-2,
  0.31416E1,
  34e1,
  0x0.1E,
  0xA23p-4,
  0X1.921FB54442D18P+1,

  0x.CD,
  0xAB.CD,
  0xAB.,
  0x.CDp1,
  0xAB.CDp1,
  0xAB.p1,
  0x.CDp-1,
  0xAB.CDp-1,
  0xAB.p-1,
  0xABp1,
  0xABp-1,

  .34,
  12.34,
  12.,
  .34e1,
  12.34e1,
  12.e1,
  .34e-1,
  12.34e-1,
  12.e-1,
  12e1,
  12e-1,

  0xABCD,
  1234,

  'alo\n123"',
  "alo\n123\"",
  '\97lo\10\04923"',
  [[alo
123"]],
  [==[
alo
123"]==],

  "",
  "\a\b\f\n\r\t\v\\\"\'",
  '\a\b\f\n\r\t\v\\\"\'',
  "a\
b\zc\z d\z
e\z
 f",

  "\x00\xfe\xeD\xFa\xCE",
  "\0\01\0234",

  "\u{41}\u{00000041}\u{0000000000000041}",
  "\u{2262}\u{0391}\u{002e}",
  "\u{D55C}\u{AD6D}\u{C5B4}",
  "\u{65e5}\u{672c}\u{8a9e}",
  "\u{FEFF}\u{0233B4}",

  [[foo
bar]],
  [[
foo
bar
]],
  [=[]]]=],

  true,
  false,
}

--END--

local lua_lexer = require "dromozoa.lua_lexer"
local util = require "dromozoa.util"

local lexer = lua_lexer.new(arg[0], util.read_file(arg[0]))
local tokens = lexer:lex()

local i = 0
local state = 1
for _, token in ipairs(tokens) do
  local u = token.value
  if state == 1 and token.kind == "Comment" and u == "BEGIN--" then
    state = 2
  elseif state == 2 then
    if token.kind == "Comment" and u == "END--" then
      state = 3
      break
    end

    i = i + 1
    local v = expect[i]

    if token.kind == "Integer" then
      assert(math.type(u) == "integer")
      assert(math.type(v) == "integer")
      assert(u == v)
    elseif token.kind == "Float" then
      assert(math.type(u) == "float")
      assert(math.type(v) == "float")
      assert(u == v)
    elseif token.kind == "String" then
      assert(u == v)
    elseif token.kind == "true" then
      assert(u == "true")
      assert(v == true)
    elseif token.kind == "false" then
      assert(u == "false")
      assert(v == false)
    else
      i = i - 1
    end
  end
end

assert(i == #expect)
assert(state == 3)
