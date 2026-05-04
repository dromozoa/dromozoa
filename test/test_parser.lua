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

local lexer = require "dromozoa.lexer"
local parser = require "dromozoa.parser"

local verbose = os.getenv "VERBOSE"

local p = parser.new()
p.tokens = lexer.new():lex([[
--[1]
local--[3]
--[4]
x--[6]
--[7]
=--[9]
--[10]
1--[12]
]], "=(test)")
p.index = 1
assert(#p.tokens == 13)

assert(p:peek().kind == "local")
assert(p:read().kind == "local")
assert(p:peek().kind == "Name")
assert(p:read().kind == "Name")
p:unread()
assert(p:peek().kind == "Name")
assert(p:read().kind == "Name")
p:unread()
p:unread()
assert(p:peek().kind == "local")
assert(p:peek().kind == "local")
assert(p:read().kind == "local")
assert(p:read().kind == "Name")
assert(p:peek().kind == "=")
p:unread()
p:unread()
assert(p:peek().kind == "local")
assert(p:read().kind == "local")
assert(p:read().kind == "Name")
assert(p:read().kind == "=")
assert(p:read().kind == "Integer")
assert(p:read().kind == "EOF")
assert(p:read().kind == "EOF")

---@param u dromozoa.node
---@param buffer string[]
local function dump(u, buffer)
  local n = #u.nodes
  if n > 0 then
    table.insert(buffer, "(")
  end
  if u.kind == "Integer" or u.kind == "Name" then
    table.insert(buffer, tostring(u.token.value))
  else
    table.insert(buffer, u.kind)
  end
  for _, v in ipairs(u.nodes) do
    table.insert(buffer, " ")
    dump(v, buffer)
  end
  if n > 0 then
    table.insert(buffer, ")")
  end
  return buffer
end

---@param source string
---@param expect string
---@param fn fun(p: dromozoa.parser): dromozoa.node
local function test_parse(source, expect, fn)
  local p = parser.new()
  p.tokens = lexer.new():lex(source, "=(test)")
  p.index = 1
  local root = fn(p)
  p:peek():require "EOF"
  local result = table.concat(dump(root, {}))
  assert(result == expect, ("{ source = %q, result = %q, expect = %q }"):format(source, result, expect))
end

---@param source string
---@param fn fun(p: dromozoa.parser): dromozoa.node
local function test_parse_error(source, fn)
  local p = parser.new()
  p.tokens = lexer.new():lex(source, "=(test)")
  p.index = 1
  local result, message = pcall(fn, p)
  assert(not result)
  if verbose then
    print(("="):rep(80))
    print(message)
    local result, message = load(source)
    assert(not result)
    print(message)
  end
  assert(tostring(message):find "=%(test%):1:", ("{ message = %q }"):format(message))
end

---@param source string
---@param expect string
local function test_parse_exp(source, expect)
  test_parse(source, expect, function(p)
    return p:parse_exp(0)
  end)
end

test_parse_exp("1 and 2 or 3", "(or (and 1 2) 3)")
test_parse_exp("1 or 2 and 3", "(or 1 (and 2 3))")
test_parse_exp("1 + 2 + 3", "(+ (+ 1 2) 3)")
test_parse_exp("1 ^ 2 ^ 3", "(^ 1 (^ 2 3))")

test_parse_exp("- 1 + 2 - 3", "(- (+ (- 1) 2) 3)")
test_parse_exp("- 1 ^ 2 ^ 3", "(- (^ 1 (^ 2 3)))")
test_parse_exp("not a or b", "(or (not a) b)")

test_parse_exp("1 + 2 * 3", "(+ 1 (* 2 3))")
test_parse_exp("(1 + 2) * 3", "(* (group (+ 1 2)) 3)")

test_parse_exp("a", "a")
test_parse_exp("a[1 + 2]", "(index a (+ 1 2))")
test_parse_exp("a.b", "(member a b)")
test_parse_exp("a.b.c", "(member (member a b) c)")

test_parse_exp("{}", "table")
test_parse_exp("{1}", "(table (list_field 1))")
test_parse_exp("{1,}", "(table (list_field 1))")
test_parse_exp("{1,2}", "(table (list_field 1) (list_field 2))")
test_parse_exp("{1,2,}", "(table (list_field 1) (list_field 2))")
test_parse_exp("{1,2,3}", "(table (list_field 1) (list_field 2) (list_field 3))")
test_parse_exp("{1,2,3,}", "(table (list_field 1) (list_field 2) (list_field 3))")
test_parse_exp("{a=b,c}", "(table (member_field a b) (list_field c))")

test_parse_exp("f()", "(call f arguments)")
test_parse_exp("f(a)", "(call f (arguments a))")
test_parse_exp("f(a,b)", "(call f (arguments a b))")
test_parse_exp("f{}", "(call f (arguments table))")
test_parse_exp("f{a}", "(call f (arguments (table (list_field a))))")
test_parse_exp("f{a,b}", "(call f (arguments (table (list_field a) (list_field b))))")
test_parse_exp("f[[a]]", "(call f (arguments String))")

test_parse_exp("x:f()", "(self x f arguments)")
test_parse_exp("x:f(a)", "(self x f (arguments a))")
test_parse_exp("x:f(a,b)", "(self x f (arguments a b))")

test_parse_exp("x.y.f(1,2,3)", "(call (member (member x y) f) (arguments 1 2 3))")
test_parse_exp("x.y:f(1,2,3)", "(self (member x y) f (arguments 1 2 3))")

test_parse_exp("1 + - - 2", "(+ 1 (- (- 2)))")
test_parse_exp("- - 1 + 2", "(+ (- (- 1)) 2)")
test_parse_exp("1 + - 2 ^ 3", "(+ 1 (- (^ 2 3)))")

test_parse_exp(
  "function () end",
  "(function (funcbody parameters block))")
test_parse_exp(
  "function (a) end",
  "(function (funcbody (parameters a) block))")
test_parse_exp(
  "function (a, b) end",
  "(function (funcbody (parameters a b) block))")
test_parse_exp(
  "function (a, b, ...) end",
  "(function (funcbody (parameters a b ...) block))")
test_parse_exp(
  "function (a, b, ...t) end",
  "(function (funcbody (parameters a b (... t)) block))")
test_parse_exp(
  "function (...) end",
  "(function (funcbody (parameters ...) block))")
test_parse_exp(
  "function (...t) end",
  "(function (funcbody (parameters (... t)) block))")

test_parse_exp(
  "f()(1)[2][3] * 4",
  "(* (index (index (call (call f arguments) (arguments 1)) 2) 3) 4)")

---@param source string
local function test_parse_exp_error(source)
  test_parse_error(source, function(p)
    return p:parse_exp(0)
  end)
end

test_parse_exp_error "()"
test_parse_exp_error "- +"
test_parse_exp_error "{,}"
test_parse_exp_error "{1,,}"
test_parse_exp_error "f(,)"
test_parse_exp_error "x:f 42"

---@param source string
---@param expect string
local function test_parse_stat(source, expect)
  test_parse(source, expect, function(p)
    return p:parse_stat()
  end)
end

test_parse_stat(";", "empty")
test_parse_stat("break", "break")
test_parse_stat("::L123::", "(label L123)")
test_parse_stat("goto L123", "(goto L123)")
test_parse_stat("f()", "(call (call f arguments))")
test_parse_stat("x:f()", "(call (self x f arguments))")
test_parse_stat("a.b = 42", "(assignment (varlist (member a b)) (explist 42))")
test_parse_stat(
  "do a = 1 b = 2 end",
  "(do (block (assignment (varlist a) (explist 1)) (assignment (varlist b) (explist 2))))")
test_parse_stat(
  "while true do print(42) end",
  "(while true (block (call (call print (arguments 42)))))")
test_parse_stat(
  "repeat print(42) until false",
  "(repeat (block (call (call print (arguments 42)))) false)")
test_parse_stat("if 1 then end", "(if 1 block)")
test_parse_stat("if 1 then ; end", "(if 1 (block empty))")
test_parse_stat(
  "if 1 then ::L1:: else ::L2:: end",
  "(if 1 (block (label L1)) (block (label L2)))")
test_parse_stat(
  "if 1 then ::L1:: elseif 2 then ::L2:: end",
  "\z
    (if 1 \z
      (block (label L1)) \z
      (elseif 2 \z
        (block (label L2))\z
      )\z
    )\z
  ")
test_parse_stat(
  "if 1 then ::L1:: elseif 2 then ::L2:: else ::L3:: end",
  "\z
    (if 1 \z
      (block (label L1)) \z
      (elseif 2 \z
        (block (label L2)) \z
        (block (label L3))\z
      )\z
    )\z
  ")
test_parse_stat(
  "for i = 1, 10 do print(i) end",
  "\z
    (numeric_for i (explist 1 10) \z
      (block \z
        (call (call print (arguments i)))\z
      )\z
    )\z
  ")
test_parse_stat(
  "for i = 10, 1, -1 do print(i) end",
  "\z
    (numeric_for i (explist 10 1 (- 1)) \z
      (block \z
        (call (call print (arguments i)))\z
      )\z
    )\z
  ")
test_parse_stat(
  "for k, v in pairs(t) do print(k, v) end",
  "\z
    (generic_for (namelist k v) (explist (call pairs (arguments t))) \z
      (block \z
        (call (call print (arguments k v)))\z
      )\z
    )\z
  ")
test_parse_stat(
  "for k, v in next, t, nil do print(k, v) end",
  "\z
    (generic_for (namelist k v) (explist next t nil) \z
      (block \z
        (call (call print (arguments k v)))\z
      )\z
    )\z
  ")

test_parse_stat(
  "function f() end",
  "(function f (funcbody parameters block))")
test_parse_stat(
  "function x.f() end",
  "(function (member x f) (funcbody parameters block))")
test_parse_stat(
  "function x:f() end",
  "(function (method x f) (funcbody parameters block))")
test_parse_stat(
  "function x.y.f() end",
  "(function (member (member x y) f) (funcbody parameters block))")
test_parse_stat(
  "function x.y:f() end",
  "(function (method (member x y) f) (funcbody parameters block))")

---@param source string
local function test_parse_stat_error(source)
  test_parse_error(source, function(p)
    return p:parse_stat()
  end)
end

test_parse_stat_error "::1::"
test_parse_stat_error "goto 1"
test_parse_stat_error "for i = 1 do print(i) end"
test_parse_stat_error "for i = 1, 2, 3, 4 do print(i) end"
test_parse_stat_error "function x:y:f() end"

---@param source string
---@param expect string
local function test_parse_block(source, expect)
  test_parse(source, expect, function(p)
    return p:parse_block()
  end)
end

test_parse_block("::L1::", "(block (label L1))")
test_parse_block("::L1::::L2::", "(block (label L1) (label L2))")
test_parse_block(";return", "(block empty return)")
test_parse_block(";return;", "(block empty return)")
test_parse_block(";return 1", "(block empty (return 1))")
test_parse_block(";return 1;", "(block empty (return 1))")
test_parse_block(";return 1,2", "(block empty (return 1 2))")
test_parse_block(";return 1,2;", "(block empty (return 1 2))")

---@param source string
local function test_parse_block_error(source)
  test_parse_error(source, function(p)
    return p:parse_block()
  end)
end

test_parse_block_error "return 42;;"
