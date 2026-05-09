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

local lua_lex = require "dromozoa.lua_lex"
local matcher = require "dromozoa.matcher"
local parser = require "dromozoa.parser"
local source_location = require "dromozoa.source_location"
local token_stream = require "dromozoa.token_stream"

local verbose = os.getenv "VERBOSE"

---@param source string
---@param filename string
---@return dromozoa.parser
local function new_parser(source, filename)
  return parser.new(token_stream.new(lua_lex.lex, matcher.new(source, source_location.new(filename))))
end

---@param u dromozoa.node
---@param buffer string[]
local function dump_impl(u, buffer)
  local enclose = #u.nodes > 0
      or u.attribute
      or not u:check("nil", "false", "true", "Float", "Integer", "String", "Name")
  if enclose then
    table.insert(buffer, "(")
  end

  -- https://www.lua.org/manual/5.5/readme.html#changes
  -- 浮動小数点数が、読み戻しに十分な桁数の文字列で表現されるようになった。
  if u:check("Float", "Integer", "Name") then
    table.insert(buffer, tostring(u.token.value))
  else
    table.insert(buffer, u.kind)
  end
  if u.attribute then
    table.insert(buffer, " <")
    table.insert(buffer, tostring(u.attribute.value))
    table.insert(buffer, ">")
  end
  for _, v in ipairs(u.nodes) do
    table.insert(buffer, " ")
    dump_impl(v, buffer)
  end

  if enclose then
    table.insert(buffer, ")")
  end
end

---@param u dromozoa.node
---@return string
local function dump(u)
  local buffer = {}
  dump_impl(u, buffer)
  return table.concat(buffer)
end

---@param u dromozoa.node
local function check(u, ...)
  if u.category == "block" then
    assert(u:check("chunk", "block"))
    assert(not u.token)
  end

  if u.category == "statement" then
    assert(u.token, ("{ kind = %q, srcloc = %q }"):format(u.kind, source_location.to_string(u:srcloc())))
  end

  if u.category == "expression" then
    assert(u.token, ("{ kind = %q, srcloc = %q }"):format(u.kind, source_location.to_string(u:srcloc())))
  end

  if u:check("chunk", "block") then
    assert(u.category == "block")
    assert(not u.token)
  end

  if u:check("variables", "names", "expressions", "parameters") then
    assert(u.category == "auxiliary")
    assert(not u.token)
  end

  for _, v in ipairs(u.nodes) do
    if u.category == "block" then
      assert(v.category == "statement")
    end
    if v.category == "statement" then
      assert(u.category == "block")
    end
    if u:check "expressions" then
      assert(v.category == "expression")
    end
    check(v, u, ...)
  end
end

---@param source string
---@return string
local function normalize(source)
  return (source
    :gsub("^%s+%(", "(")
    :gsub("%)%s+$", ")")
    :gsub("%s+%(", " (")
    :gsub("%)%s+", ") ")
    :gsub("%s+%)", ")")
  )
end

---@param source string
---@param expect string
---@param fn fun(p: dromozoa.parser): dromozoa.node
---@return dromozoa.node
local function test_parse_impl(source, expect, fn)
  local p = new_parser(source, "=(test)")
  local root = fn(p)
  p:peek():require "EOF"
  local result = dump(root)
  local expect = normalize(expect)
  assert(result == expect, ("{ source = %q, result = %q, expect = %q }"):format(source, result, expect))
  check(root)
  return root
end

---@param source string
---@param fn fun(p: dromozoa.parser): dromozoa.node
local function test_parse_error_impl(source, fn)
  local p = new_parser(source, "=(test)")
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
  local root = test_parse_impl(source, expect, function(p)
    return p:parse_exp()
  end)
  assert(root.category == "expression")
end

test_parse_exp("1 and 2 or 3", "(or (and 1 2) 3)")
test_parse_exp("1 or 2 and 3", "(or 1 (and 2 3))")
test_parse_exp("1 + 2 + 3", "(+ (+ 1 2) 3)")
test_parse_exp("1 ^ 2 ^ 3", "(^ 1 (^ 2 3))")
test_parse_exp("a .. b .. c", "(.. a (.. b c))")

test_parse_exp("- 1 + 2 - 3", "(- (+ (- 1) 2) 3)")
test_parse_exp("- 1 ^ 2 ^ 3", "(- (^ 1 (^ 2 3)))")
test_parse_exp("not a or b", "(or (not a) b)")

test_parse_exp("1 + 2 * 3", "(+ 1 (* 2 3))")
test_parse_exp("(1 + 2) * 3", "(* (group (+ 1 2)) 3)")

test_parse_exp("a", "a")
test_parse_exp("a[1 + 2]", "(index a (+ 1 2))")
test_parse_exp("a.b", "(member a b)")
test_parse_exp("a.b.c", "(member (member a b) c)")

test_parse_exp("{}", "(table)")
test_parse_exp("{1}", "(table (list_field 1))")
test_parse_exp("{1,}", "(table (list_field 1))")
test_parse_exp("{1,2}", "(table (list_field 1) (list_field 2))")
test_parse_exp("{1,2,}", "(table (list_field 1) (list_field 2))")
test_parse_exp("{1,2,3}", "(table (list_field 1) (list_field 2) (list_field 3))")
test_parse_exp("{1,2,3,}", "(table (list_field 1) (list_field 2) (list_field 3))")
test_parse_exp("{a=b,c}", "(table (member_field a b) (list_field c))")
test_parse_exp("{[a]=b,c}", "(table (index_field a b) (list_field c))")

test_parse_exp("f()", "(call f (expressions))")
test_parse_exp("f(a)", "(call f (expressions a))")
test_parse_exp("f(a,b)", "(call f (expressions a b))")
test_parse_exp("f{}", "(call f (expressions (table)))")
test_parse_exp("f{a}", "(call f (expressions (table (list_field a))))")
test_parse_exp("f{a,b}", "(call f (expressions (table (list_field a) (list_field b))))")
test_parse_exp("f[[a]]", "(call f (expressions String))")

test_parse_exp("x:f()", "(self x f (expressions))")
test_parse_exp("x:f(a)", "(self x f (expressions a))")
test_parse_exp("x:f(a,b)", "(self x f (expressions a b))")

test_parse_exp("x.y.f(1,2,3)", "(call (member (member x y) f) (expressions 1 2 3))")
test_parse_exp("x.y:f(1,2,3)", "(self (member x y) f (expressions 1 2 3))")

test_parse_exp("1 + - - 2", "(+ 1 (- (- 2)))")
test_parse_exp("- - 1 + 2", "(+ (- (- 1)) 2)")
test_parse_exp("1 + - 2 ^ 3", "(+ 1 (- (^ 2 3)))")

test_parse_exp(
  "function () end",
  "(function (body (parameters) (block)))")
test_parse_exp(
  "function (a) end",
  "(function (body (parameters a) (block)))")
test_parse_exp(
  "function (a, b) end",
  "(function (body (parameters a b) (block)))")
test_parse_exp(
  "function (a, b, ...) end",
  "(function (body (parameters a b (...)) (block)))")
test_parse_exp(
  "function (a, b, ...t) end",
  "(function (body (parameters a b (... t)) (block)))")
test_parse_exp(
  "function (...) end",
  "(function (body (parameters (...)) (block)))")
test_parse_exp(
  "function (...t) end",
  "(function (body (parameters (... t)) (block)))")

test_parse_exp(
  "f()(1)[2][3] * 4",
  "(* (index (index (call (call f (expressions)) (expressions 1)) 2) 3) 4)")

test_parse_exp("4.25", "4.25")

---@param source string
local function test_parse_exp_error(source)
  test_parse_error_impl(source, function(p)
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
  local root = test_parse_impl(source, expect, function(p)
    return p:parse_stat()
  end)
  assert(root.category == "statement")
end

test_parse_stat("f()", "(call (call f (expressions)))")
test_parse_stat("x:f()", "(call (self x f (expressions)))")

test_parse_stat("a.b = 42", [[
  (assignment
    (variables (member a b))
    (expressions 42)
  )
]])
test_parse_stat("a.b, c[d + e], f().g = 42, 69", [[
  (assignment
    (variables (member a b) (index c (+ d e)) (member (call f (expressions)) g))
    (expressions 42 69)
  )
]])

test_parse_stat(";", "(empty)")
test_parse_stat("::L123::", "(label L123)")
test_parse_stat("break", "(break)")
test_parse_stat("goto L123", "(goto L123)")

test_parse_stat("do a = 1 b = 2 end", [[
  (do
    (block
      (assignment (variables a) (expressions 1))
      (assignment (variables b) (expressions 2))
    )
  )
]])

test_parse_stat("while true do print(42) end", [[
  (while true
    (block (call (call print (expressions 42))))
  )
]])

test_parse_stat("repeat print(42) until false", [[
  (repeat
    (block (call (call print (expressions 42))))
    false
  )
]])

test_parse_stat("if x then print(1) end", [[
  (if x
    (block (call (call print (expressions 1))))
  )
]])
test_parse_stat("if x then print(1) else print(2) end", [[
  (if x
    (block (call (call print (expressions 1))))
    (block (call (call print (expressions 2))))
  )
]])
test_parse_stat("if x then print(1) elseif y then print(2) end", [[
  (if x
    (block (call (call print (expressions 1))))
    (block
      (if y
        (block (call (call print (expressions 2))))
      )
    )
  )
]])
test_parse_stat("if x then print(1) elseif y then print(2) else print(3) end", [[
  (if x
    (block (call (call print (expressions 1))))
    (block
      (if y
        (block (call (call print (expressions 2))))
        (block (call (call print (expressions 3))))
      )
    )
  )
]])

test_parse_stat("for i = 1, 10 do print(i) end", [[
  (numeric_for i (expressions 1 10)
    (block
      (call (call print (expressions i)))
    )
  )
]])
test_parse_stat("for i = 10, 1, -1 do print(i) end", [[
  (numeric_for i (expressions 10 1 (- 1))
    (block
      (call (call print (expressions i)))
    )
  )
]])

test_parse_stat("for k, v in pairs(t) do print(k, v) end", [[
  (generic_for (names k v) (expressions (call pairs (expressions t)))
    (block
      (call (call print (expressions k v)))
    )
  )
]])
test_parse_stat("for k, v in next, t, nil do print(k, v) end", [[
  (generic_for (names k v) (expressions next t nil)
    (block
      (call (call print (expressions k v)))
    )
  )
]])

test_parse_stat(
  "function f() end",
  "(function f (body (parameters) (block)))")
test_parse_stat(
  "function x.f() end",
  "(function (member x f) (body (parameters) (block)))")
test_parse_stat(
  "function x:f() end",
  "(function (method x f) (body (parameters) (block)))")
test_parse_stat(
  "function x.y.f() end",
  "(function (member (member x y) f) (body (parameters) (block)))")
test_parse_stat(
  "function x.y:f() end",
  "(function (method (member x y) f) (body (parameters) (block)))")

test_parse_stat("local function f(x) if x > 0 then return f(x - 1) end end", [[
  (local_function f
    (body (parameters x)
      (block
        (if (> x 0)
          (block
            (return (expressions (call f (expressions (- x 1)))))
          )
        )
      )
    )
  )
]])

test_parse_stat(
  "local x",
  "(local (names x))")
test_parse_stat(
  "local x <const>",
  "(local (names (x <const>)))")
test_parse_stat(
  "local x <const>, y <close>",
  "(local (names (x <const>) (y <close>)))")
test_parse_stat(
  "local <const> x",
  "(local (names <const> x))")
test_parse_stat(
  "local <const> x <const>",
  "(local (names <const> (x <const>)))")
test_parse_stat(
  "local <const> x <const>, y <close>",
  "(local (names <const> (x <const>) (y <close>)))")

test_parse_stat(
  "local x, y, z = true, 42, 'foo'",
  "(local (names x y z) (expressions true 42 String))")

test_parse_stat("global function f(x) if x > 0 then return f(x - 1) end end", [[
  (global_function f
    (body (parameters x)
      (block
        (if (> x 0)
          (block
            (return (expressions (call f (expressions (- x 1)))))
          )
        )
      )
    )
  )
]])

test_parse_stat(
  "global x",
  "(global (names x))")
test_parse_stat(
  "global x <const>",
  "(global (names (x <const>)))")
test_parse_stat(
  "global x <const>, y <close>",
  "(global (names (x <const>) (y <close>)))")
test_parse_stat(
  "global <const> x",
  "(global (names <const> x))")
test_parse_stat(
  "global <const> x <const>",
  "(global (names <const> (x <const>)))")
test_parse_stat(
  "global <const> x <const>, y <close>",
  "(global (names <const> (x <const>) (y <close>)))")

test_parse_stat(
  "global x, y, z = true, 42, 'foo'",
  "(global (names x y z) (expressions true 42 String))")

test_parse_stat("global *", "(global (any))")
test_parse_stat("global <const> *", "(global (any <const>))")

---@param source string
local function test_parse_stat_error(source)
  test_parse_error_impl(source, function(p)
    return p:parse_stat()
  end)
end

test_parse_stat_error "::1::"
test_parse_stat_error "goto 1"
test_parse_stat_error "for i = 1 do print(i) end"
test_parse_stat_error "for i = 1, 2, 3, 4 do print(i) end"
test_parse_stat_error "function x:y:f() end"
test_parse_stat_error "local *"
test_parse_stat_error "local <const> *"

---@param source string
---@param expect string
local function test_parse_block(source, expect)
  test_parse_impl(source, expect, function(p)
    return p:parse_block()
  end)
end

test_parse_block("::L1::", "(block (label L1))")
test_parse_block("::L1::::L2::", "(block (label L1) (label L2))")
test_parse_block(";return", "(block (empty) (return (expressions)))")
test_parse_block(";return;", "(block (empty) (return (expressions)))")
test_parse_block(";return 1", "(block (empty) (return (expressions 1)))")
test_parse_block(";return 1;", "(block (empty) (return (expressions 1)))")
test_parse_block(";return 1,2", "(block (empty) (return (expressions 1 2)))")
test_parse_block(";return 1,2;", "(block (empty) (return (expressions 1 2)))")

---@param source string
local function test_parse_block_error(source)
  test_parse_error_impl(source, function(p)
    return p:parse_block()
  end)
end

test_parse_block_error "return 42;;"

---@param source string
---@param expect string
local function test_parse(source, expect)
  local p = new_parser(source, "=(test)")
  local root = p:parse()
  local result = dump(root)
  local expect = normalize(expect)
  assert(result == expect, ("{ source = %q, result = %q, expect = %q }"):format(source, result, expect))
  assert(root:check "chunk")
  assert(root.category == "block")
end

test_parse([[
local class = {}

function class.read_file(filename)
  local handle<close> = assert(io.open(filename, "rb"))
  return handle:read "a"
end

io.write(read_file((...)))
]], [[
(chunk
  (local (names class) (expressions (table)))

  (function (member class read_file)
    (body (parameters filename)
      (block
        (local (names (handle <close>))
          (expressions
            (call assert
              (expressions
                (call (member io open) (expressions filename String))
              )
            )
          )
        )
        (return (expressions (self handle read (expressions String))))
      )
    )
  )

  (call
    (call (member io write)
      (expressions
        (call read_file (expressions (group (...))))
      )
    )
  )
)
]])
