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

--SRCLOC:748,20,1--
; --SRCLOC:770,21,3--
--[[
foo
bar
baz]] --SRCLOC:809,25,7--

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

local lexer = require "dromozoa.lexer"
local util = require "dromozoa.util"

local verbose = os.getenv "VERBOSE"

local source = util.read_file(arg[0])
local tokens = lexer.new():lex(source, arg[0])

local i = 0
local state = 1
local buffer = {}
for _, token in ipairs(tokens) do
  local u = token.value
  if state == 1 then
    if token.kind == "Comment" and u == "BEGIN--" then
      state = 2
    end
  elseif state == 2 then
    if token.kind == "Comment" then
      if u == "END--" then
        state = 3
      else
        local position, line, column = u:match "^SRCLOC:(%d+),(%d+),(%d+)%-%-$"
        if position then
          assert(token.srcloc.position == tonumber(position))
          assert(token.srcloc.line == tonumber(line))
          assert(token.srcloc.column == tonumber(column))
        end
      end
    else
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
  table.insert(buffer, token.text)
end

assert(i == #expect)
assert(state == 3)
assert(table.concat(buffer) == source)

---@param source string
local function test_lex_error(source)
  local result, message = pcall(function() lexer.new():lex(source, "=test") end)
  assert(not result)
  if verbose then
    print(message)
  end
  assert(assert(message):find "=test:1:", ("{ message = %q }"):format(message))
end

test_lex_error "print(--[["
test_lex_error [[print "\y"]]
test_lex_error "print([["
test_lex_error "!"

local tokens = lexer.new():lex("", "=test")
assert(#tokens == 1)

local token = tokens[1]
assert(token.kind == "EOF")

local tokens = lexer.new():lex("#! /usr/bin/env lua\n", "=test")
assert(#tokens == 2)

local token = tokens[1]
assert(token.kind == "Comment")
assert(token.subkind == "Shebang")
assert(token.text == "#! /usr/bin/env lua\n")
assert(token.value == "! /usr/bin/env lua")
