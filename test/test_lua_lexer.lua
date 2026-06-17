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

---@diagnostic disable: unused-label
::BEGIN::

::SRCLOC_787_21_3::
; ::SRCLOC_809_22_5::
--[[
foo
bar
baz]]
assert(type(type)); ::SRCLOC_868_27_23::

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

::END::

local lua_lexer = require "dromozoa.lua_lexer"
local matcher = require "dromozoa.matcher"
local source_location = require "dromozoa.source_location"
local token_stream = require "dromozoa.token_stream"
local util = require "dromozoa.util"

local verbose = os.getenv "VERBOSE"

---@param source string
---@param filename string
---@return dromozoa.token_stream
local function new_lexer(source, filename)
  return token_stream.new(lua_lexer.lex, matcher.new(source, source_location.new(filename)))
end

local filename = arg[0]
local source = util.read_file(filename)
local lexer = new_lexer(source, filename)

local i = 0
local state = 1
local is_preceeded_by_label = false
while true do
  local token = lexer:read()
  local u = token.value
  if state == 1 then
    if is_preceeded_by_label and token:check "Name" and u == "BEGIN" then
      state = 2
    end
  elseif state == 2 then
    if is_preceeded_by_label and token:check "Name" then
      if u == "END" then
        state = 3
      else
        local position, line, column = u:match "^SRCLOC_(%d+)_(%d+)_(%d+)$"
        if position then
          assert(token.first_srcloc.position == tonumber(position))
          assert(token.first_srcloc.line == tonumber(line))
          assert(token.first_srcloc.column == tonumber(column))
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
  if token:check "EOF" then
    break
  end
  is_preceeded_by_label = token:check "::"
end
assert(i == #expect)
assert(state == 3)

local buffer = {}
for _, x in ipairs(lexer.tokens) do
  table.insert(buffer, x.text)
end
assert(table.concat(buffer) == source)

---@param source string
---@param column integer
---@param expect string?
local function test_lex_error(source, column, expect)
  local result, message = pcall(function()
    local lexer = new_lexer(source, "=(test)")
    repeat
      local token = lexer:read()
    until token:check "EOF"
  end)
  assert(not result)
  assert(message)
  if verbose then
    print(("="):rep(80))
    print(message)
    local result, message = load(source)
    assert(not result)
    assert(message)
    print(message)
  end
  assert(message:find("=%(test%):1:" .. column .. "$"), ("{ message = %q }"):format(message))
  if expect then
    assert(message:find(expect, 1, true), ("{ message = %q }"):format(message))
  end
end

test_lex_error("print(--[[abc", 11, "unfinished long comment")
test_lex_error([[print "abc]], 11, "unfinished string")
test_lex_error([[print 'abc]], 11, "unfinished string")
test_lex_error('print "abc\n"', 11, "unfinished string")
test_lex_error([[print "abc\y"]], 11, "invalid escape sequence")
test_lex_error("print([[abc", 9, "unfinished long string")
test_lex_error("print([=[abc", 10, "unfinished long string")
test_lex_error("abc!", 4)

test_lex_error("print[====]", 11)

local lexer = new_lexer("", "=(test)")
lexer:read():require "EOF"
assert(#lexer.tokens == 1)

local lexer = new_lexer("#! /usr/bin/env lua", "=(test)")
lexer:read():require "EOF"
assert(#lexer.tokens == 2)
local token = lexer.tokens[1]
assert(token:require "Comment")
assert(token.subkind == "Shebang")
assert(token.text == "#! /usr/bin/env lua")
assert(token.value == "! /usr/bin/env lua")

local lexer = new_lexer("#! /usr/bin/env lua\n#", "=(test)")
lexer:read():require "#"
lexer:read():require "EOF"
assert(#lexer.tokens == 4)
local token = lexer.tokens[1]
assert(token:require "Comment")
assert(token.subkind == "Shebang")
assert(token.text == "#! /usr/bin/env lua")
assert(token.value == "! /usr/bin/env lua")

local lexer = new_lexer("---@alias x integer", "=(test)")
lexer:read():require "EOF"
assert(#lexer.tokens == 2)

local lexer = new_lexer("---@alias x integer\n", "=(test)")
lexer:read():require "EOF"
assert(#lexer.tokens == 3)

-- matcherのoffsetの扱いをテストする
local source = "xyz"
local srcloc = source_location.new "=(test)"
srcloc.position = 2
srcloc.line = 1
srcloc.column = 2
local lexer = token_stream.new(lexer.lex, matcher.new(source, srcloc))
lexer:read():require "Name"
lexer:read():require "EOF"
assert(#lexer.tokens == 2)
assert(lexer.tokens[1].text == source)
