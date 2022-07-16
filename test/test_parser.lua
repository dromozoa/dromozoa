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


  lr0_items()

  lalr1_items()
    set_of_items, transitions = lr0_items()
    set_of_items = lalr1_kernels(set_of_items, transitions)
    for set_of_items
      lr1_closure(set_of_item)
    return set_of_items, transitions





  set_of_items, transitions = lalr1_items()
  parser, conflicts = lr1_construct_table(set_of_items, transitions)



  production = { ... }
  item = { id = production_id, dot = ?, la = ... }

  productions

  production = {
    head = symbol;
    body = { symbol, ... };
  }

  item = {
    index = production_index;
    dot = position;
  }


  map_of_production_indices
    head -> index...
    あるheadを持つproductionの集合をえる

  productionsは、リスト構造と、headでの検索を行いたい

  symbol
    1 .. max_terminal_symbol    terminal_symbol
    max_terminal_symbol+1 .. #  nonterminal_symbol

  symbol_names = {...}
  1: min_terminal_symbol = 1
  m: max_terminal_symbol
     min_nonterminal_symbol = max_terminal_symbol + 1
  n: max_nonterminal_symbol = #

]]

local module = {}

---------------------------------------------------------------------------

local function equal(a, b)
  if rawequal(a, b) then
    return true
  end
  if type(a) == "table" and type(b) == "table" then
    for k, u in pairs(a) do
      if not equal(u, b[k]) then
        return false
      end
    end
    for k in pairs(b) do
      if a[k] == nil then
        return false
      end
    end
    return true
  end
  return false
end

module.equal = equal

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.productions" }

function class:add(head, body)
  self[#self + 1] = { head = head, body = body }
  return self
end

function class:each_by_head(symbol)
  return function (self, index)
    index = index or 0
    for i = index + 1, #self do
      local production = self[i]
      local head = production.head

      if head == symbol then
        return i, head, production.body
      end
    end
  end, self
end

module.productions = setmetatable(class, {
  __call = function ()
    return setmetatable({}, metatable)
  end;
})

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.items", __eq = module.equal }

function class:add(index, dot, la)
  self[#self + 1] = { index = index, dot = dot, la = la }
  return self
end

module.items = setmetatable(class, {
  __call = function ()
    return setmetatable({}, metatable)
  end;
})

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.set" }

function class:find(v)
  for i, u in ipairs(self) do
    if u == v then
      return i, u
    end
  end
end

function class:add(v)
  assert(not self:find(v))
  local n = #self + 1
  self[n] = v
  return n
end

module.set = setmetatable(class, {
  __call = function ()
    return setmetatable({}, metatable)
  end;
})

---------------------------------------------------------------------------

local private = setmetatable({}, { __mode = "k" })
local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.map" }

function class:each()
  local priv = private[self]
  return coroutine.wrap(function ()
    for i = 1, #priv do
      local k = priv[i]
      coroutine.yield(k, self[k])
    end
  end), self
end

function metatable:__newindex(k, v)
  local priv = private[self]
  priv[#priv + 1] = k
  rawset(self, k, v)
end

module.map = setmetatable(class, {
  __call = function ()
    local self = setmetatable({}, metatable)
    private[self] = {}
    return self
  end;
})

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.list" }

function class:add(v)
  self[#self + 1] = v
  return self
end

module.list = setmetatable(class, {
  __call = function ()
    return setmetatable({}, metatable)
  end;
})

---------------------------------------------------------------------------

-- キーにひもづけられた値がなければ、テーブルを作成して設定する
-- キーにひもづけられた値を返す

-- キーにひもづけられた値がなければ、コンストラクタを実行して設定する
-- キーにひもづけられた値を返す

local function optional_get(t, k, fn, ...)
  local v = t[k]
  if v == nil then
    if fn == nil then
      v = {}
    else
      v = fn(...)
    end
    t[k] = v
  end
  return v
end

---------------------------------------------------------------------------

local first_symbols

-- P.221
local function first_symbol(grammar, symbol)
  if symbol <= grammar.max_terminal_symbol then
    return { [symbol] = true }
  else
    local productions = grammar.productions
    local first = {}
    for i, head, body in productions:each_by_head(symbol) do
      if body[1] then -- is not epsilon
        for symbol in pairs(first_symbols(grammar, body)) do
          first[symbol] = true
        end
      else
        first[0] = true -- epsilon
      end
    end
    return first
  end
end

function first_symbols(grammar, symbols)
  local first = {}
  for i, symbol in ipairs(symbols) do
    for symbol in pairs(first_symbol(grammar, symbol)) do
      first[symbol] = true
    end
    if first[0] then -- epsilon
      first[0] = nil
    else
      return first
    end
  end
  first[0] = true
  return first
end

local function first(grammar)
  local first_table = {}
  for symbol = grammar.max_terminal_symbol + 1, grammar.max_nonterminal_symbol do
    first_table[symbol] = first_symbol(grammar, symbol)
  end
  return first_table
end

-- P.245
local function lr0_closure(grammar, items)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol

  local added = {}
  local m = 1
  while true do
    local n = #items
    if m > n then
      break
    end
    for i = m, n do
      local item = items[i]
      local symbol = productions[item.index].body[item.dot]
      if symbol and symbol > max_terminal_symbol and not added[symbol] then
        for j in productions:each_by_head(symbol) do
          items:add(j, 1)
        end
        added[symbol] = true
      end
    end
    m = n + 1
  end
end

-- goto結果を直接addしてもよいのでは？
local function lr0_goto(grammar, items)
  local productions = grammar.productions

  local map_of_to_items = module.map()

  for _, item in ipairs(items) do
    local index = item.index
    local dot = item.dot
    local symbol = productions[index].body[dot]
    if symbol then
      optional_get(map_of_to_items, symbol, module.items):add(index, dot + 1)
    end
  end

  local gotos = {}
  for symbol, to_items in map_of_to_items:each() do
    lr0_closure(grammar, to_items)
    gotos[#gotos + 1] = { symbol = symbol, to_items = to_items }
  end

  return gotos
end

-- P.246
local function lr0_items(grammar)
  local start_items = module.items():add(1, 1)
  lr0_closure(grammar, start_items)
  local set_of_items = module.set()
  set_of_items:add(start_items)
  local transitions = {}

  local m = 1
  while true do
    local n = #set_of_items
    if m > n then
      break
    end
    for i = m, n do
      local gotos = lr0_goto(grammar, set_of_items[i])
      -- transitionをset_of_itemsに統合してもよいか？
      local transition = {}
      transitions[i] = transition
      for _, data in ipairs(gotos) do
        local to_items = data.to_items
        local to = set_of_items:find(to_items)
        if not to then
          to = set_of_items:add(to_items)
        end
        transition[data.symbol] = to
      end
    end
    m = n + 1
  end
  return set_of_items, transitions
end

-- P261
-- TODO cache
local function lr1_closure(grammar, first_table, items)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol

  local added = {}
  local m = 1
  while true do
    local n = #items
    if m > n then
      break
    end
    for i = m, n do
      local item = items[i]
      -- [A -> alpha . B beta, a]
      -- FIRST(beta a)
      -- first_symbolsじゃだめなの？
      local first = {}
      local body = productions[item.index].body
      for j = item.dot + 1, #body + 1 do
        local symbol = body[j]
        if symbol then
          if symbol <= max_terminal_symbol then
            first[symbol] = true
            break
          else
            for symbol in pairs(first_table[symbol]) do
              first[symbol] = true
            end
            if first[0] then -- epsilon
              first[0] = nil
            else
              break
            end
          end
        else
          first[item.la] = true
        end
      end

      local symbol = productions[item.index].body[item.dot]
      for j in productions:each_by_head(symbol) do
        local a = added[j]
        if not a then
          a = {}
          added[j] = a
        end

        for la in pairs(first) do
          if not a[la] then
            a[la] = true
            items:add(j, 1, la)
          end
        end
      end
    end
    m = n + 1
  end
end

local function lalr1_kernels(grammar, first_table, set_of_items, transitions)
  local productions = grammar.productions
  local min_nonterminal_symbol = grammar.max_terminal_symbol + 1

  local set_of_kernel_items = {}
  local map_of_kernel_items = {}

  -- カーネル項の抽出
  for i, items in ipairs(set_of_items) do
    local kernel_items = module.items()
    local kernel_table = {}
    for j, item in ipairs(items) do
      local index = item.index
      local dot = item.dot
      if index == 1 or dot > 1 then
        -- カーネル項の生成規則ごとに、dotごとに項の参照をつくる
        optional_get(kernel_table, index)[dot] = j
      end
      if index == 1 and dot == 1 then
        kernel_items:add(index, dot, { true }) -- la = { [marker_end] = true }
      else
        kernel_items:add(index, dot, {})
      end
    end
    set_of_kernel_items[i] = kernel_items
    map_of_kernel_items[i] = kernel_table
  end

  local propagated = {}

  for i, from_items in ipairs(set_of_items) do
    for j, from_item in ipairs(from_items) do
      local from_index = from_item.index
      local from_dot = from_item.dot
      if productions[from_index].head == min_nonterminal_symbol or from_dot > 1 then
        local items = module.items():add(from_index, from_dot, -1) -- la = marker_lookahead
        lr1_closure(grammar, first_table, items)
        for _, item in ipairs(items) do
          local index = item.index
          local production = productions[id]
          local dot = item.dot
          local symbol = production.body[dot]
          if symbol then
            local la = item.la
            local to_i = transitions[i][symbol]
            local to_j = map_of_kernel_items[to_i][index][dot + 1]
            if la == -1 then -- marker_lookahead
              propagated[#propagated + 1] = {
                from_i = i;
                from_j = j;
                to_i = to_i;
                to_j = to_j;
              }
            else
              set_of_items[to_i][to_j].la[la] = true
            end
          end
        end
      end
    end
  end

end

---------------------------------------------------------------------------

local symbol_names = { "+", "*", "(", ")", "id", "E", "E'", "T", "T'", "F" }
local max_terminal_symbol = 5

local symbol_map = {}
for symbol, name in ipairs(symbol_names) do
  symbol_map[name] = symbol
end
local function _(name)
  return assert(symbol_map[name])
end

local productions = module.productions()
  :add(_"E", { _"T", _"E'" })
  :add(_"E'", { _"+", _"T", _"E'" })
  :add(_"E'", {})
  :add(_"T", { _"F", _"T'" })
  :add(_"T'", { _"*", _"F", _"T'" })
  :add(_"T'", {})
  :add(_"F", { _"(", _"E", _")" })
  :add(_"F", { _"id" })

local grammar = {
  productions = productions;
  max_terminal_symbol = max_terminal_symbol;
  max_nonterminal_symbol = #symbol_names;
}
symbol_names[0] = "epsilon"

local function dump(symbol_name, first)
  io.write(symbol_name, " => {")
  for symbol in pairs(first) do
    io.write(" ", symbol_names[symbol])
  end
  io.write " }\n"
end

dump("F", first_symbol(grammar, _"F"))
dump("T", first_symbol(grammar, _"T"))
dump("E", first_symbol(grammar, _"E"))
dump("E'", first_symbol(grammar, _"E'"))
dump("T'", first_symbol(grammar, _"T'"))

print(("="):rep(75))

---------------------------------------------------------------------------

local symbol_names = { "$", "c", "d", "S'", "S", "C" }
local max_terminal_symbol = 3

local symbol_map = {}
for symbol, name in ipairs(symbol_names) do
  symbol_map[name] = symbol
end
local function _(name)
  return assert(symbol_map[name])
end

local productions = module.productions()
  :add(_"S'", { _"S" })
  :add(_"S", { _"C", _"C" })
  :add(_"C", { _"c", _"C" })
  :add(_"C", { _"d" })

local grammar = {
  productions = productions;
  max_terminal_symbol = max_terminal_symbol;
  max_nonterminal_symbol = #symbol_names;
}
symbol_names[0] = "epsilon"

local first_table = first(grammar)
local start_items = module:items():add(1, 1, 1)
lr1_closure(grammar, first_table, start_items)

for _, item in ipairs(start_items) do
  local dot = item.dot
  local p = productions[item.index]
  local body = p.body
  io.write(symbol_names[p.head], " ->")
  for j = 1, #body do
    if j == dot then
      io.write " ."
    end
    io.write(" ", symbol_names[body[j]])
  end
  if dot == #body + 1 then
    io.write " ."
  end
  if item.la then
    io.write(", ", symbol_names[item.la])

  end
  io.write "\n"
end

print(("="):rep(75))

---------------------------------------------------------------------------

local symbol_names = { "$", "+", "*", "(", ")", "id" }
local max_terminal_symbol = #symbol_names
symbol_names[#symbol_names + 1] = "E'"
symbol_names[#symbol_names + 1] = "E"
symbol_names[#symbol_names + 1] = "T"
symbol_names[#symbol_names + 1] = "F"

local symbol_map = {}

for symbol, name in ipairs(symbol_names) do
  symbol_map[name] = symbol
end

local function _(name)
  return assert(symbol_map[name])
end

local productions = module.productions()
  :add(_"E'", { _"E" })
  :add(_"E", { _"E", _"+", _"T" })
  :add(_"E", { _"T" })
  :add(_"T", { _"T", _"*", _"F" })
  :add(_"T", { _"F" })
  :add(_"F", { _"(", _"E", _")" })
  :add(_"F", { _"id" })

local grammar = {
  productions = productions;
  max_terminal_symbol = max_terminal_symbol;
  max_nonterminal_symbol = #symbol_names;
}
-- local first_table = first(grammar)

local items = module.items()
  :add(1, 1)
lr0_closure(grammar, items)

local items = module.items()
  :add(1, 2)
  :add(2, 2)
local gotos = lr0_goto(grammar, items)
for i, data in ipairs(gotos) do
  for _, to_item in ipairs(data.to_items) do
    print(symbol_names[data.symbol], to_item.index, to_item.dot)
  end
end

print(("="):rep(75))

local set_of_items, transitions = lr0_items(grammar)
-- lalr1_kernels(grammar, first_table, set_of_items, transitions)

--[==[
for i, items in ipairs(set_of_items) do
  io.write("I_", i, "\n")
  for _, item in ipairs(items) do
    local dot = item.dot
    local p = productions[item.index]
    local body = p.body
    io.write(symbol_names[p.head], " ->")
    for j = 1, #body do
      if j == dot then
        io.write " ."
      end
      io.write(" ", symbol_names[body[j]])
    end
    if dot == #body + 1 then
      io.write " ."
    end
    io.write "\n"
  end
  -- print(("-"):rep(75))
  for symbol, to in pairs(transitions[i]) do
    io.write("==== ", symbol_names[symbol], " ===> ", "I_", to, "\n")
  end
  print(("-"):rep(75))
end
]==]

