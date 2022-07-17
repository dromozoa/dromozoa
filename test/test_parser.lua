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

local dumper = require "dromozoa.commons.dumper"

local module = {}

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.set" }

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

function class:find(v)
  for i, u in ipairs(self) do
    if equal(u, v) then
      return i, u
    end
  end
end

function class:put(v)
  local n = self:find(v)
  if n then
    return n
  else
    n = #self + 1
    self[n] = v
    return n
  end
  return n
end

module.set = setmetatable(class, {
  __call = function (_, ...)
    return setmetatable({...}, metatable)
  end;
})

---------------------------------------------------------------------------

local private = setmetatable({}, { __mode = "k" })
local class = {}
local metatable = { __name = "dromozoa.parser.map" }

function class:each()
  local priv = private[self]
  return coroutine.wrap(function ()
    for i = 1, #priv do
      local k = priv[i]
      coroutine.yield(k, self[k])
    end
  end), self
end

function metatable:__index(k)
  local v = class[k]
  if v ~= nil then
    return v
  end
  -- いちおうチェック
  assert(type(k) == "number")
  local priv = private[self]
  local v = priv.constructor()
  priv[#priv + 1] = k
  rawset(self, k, v)
  return v
end

-- 削除は考慮していない
function metatable:__newindex(k, v)
  local priv = private[self]
  priv[#priv + 1] = k
  rawset(self, k, v)
end

module.map = setmetatable(class, {
  __call = function (_, constructor)
    local self = setmetatable({}, metatable)
    private[self] = { constructor = constructor or function () return {} end }
    return self
  end;
})

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.list" }

function class:add(...)
  local n = #self
  for i, v in ipairs {...} do
    self[n + i] = v
  end
  return self
end

function class:each(fn)
  return function (self, index)
    for i = index + 1, #self do
      local v = fn(self[i])
      if v then
        return i, v
      end
    end
  end, self, 0
end

module.list = setmetatable(class, {
  __call = function (_, ...)
    return setmetatable({...}, metatable)
  end;
})

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.production" }

module.production = setmetatable(class, {
  __call = function (_, head, body)
    return setmetatable({ head = head, body = body }, metatable)
  end;
})

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.item" }

module.item = setmetatable(class, {
  __call = function (_, index, dot, la)
    return setmetatable({ index = index, dot = dot, la = la }, metatable)
  end;
})

---------------------------------------------------------------------------

local Production = module.production
local Item = module.item

---------------------------------------------------------------------------

local function eliminate_left_recursion(grammar, symbol_names)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol
  local max_nonterminal_symbol = grammar.max_nonterminal_symbol

  local new_symbol_names = module.list(table.unpack(symbol_names))
  local map_of_productions = module.map()

  for i = max_terminal_symbol + 1, max_nonterminal_symbol do
    local left_recursions = module.list()
    local no_left_recursions = module.list()

    for _, body in productions:each(function (v) return v.head == i and v.body end) do
      local symbol = body[1]
      if symbol and symbol > max_terminal_symbol and symbol < i then
        for _, production in ipairs(map_of_productions[symbol]) do
          local src_body = production.body
          local new_body = module.list(table.unpack(src_body)):add(table.unpack(body, 2))
          if i == new_body[1] then
            left_recursions:add(Production(i, new_body))
          else
            no_left_recursions:add(Production(i, new_body))
          end
        end
      else
        if i == body[1] then
          left_recursions:add(Production(i, body))
        else
          no_left_recursions:add(Production(i, body))
        end
      end
    end

    if left_recursions[1] then
      new_symbol_names:add(symbol_names[i] .. "'")
      local n = #new_symbol_names

      local productions = module.list()
      for _, left_recursion in ipairs(left_recursions) do
        local src_body = left_recursion.body
        local new_body = module.list(table.unpack(src_body, 2)):add(n)
        productions:add(Production(n, new_body))
      end
      productions:add(Production(n, {}))
      map_of_productions[n] = productions

      local productions = module.list()
      for _, no_left_recursion in ipairs(no_left_recursions) do
        local src_body = no_left_recursion.body
        local new_body = module.list(table.unpack(src_body)):add(n)
        productions:add(Production(i, new_body))
      end
      map_of_productions[i] = productions
    else
      map_of_productions[i] = no_left_recursions
    end
  end

  local new_productions = module.list()
  for i = grammar.max_terminal_symbol + 1, #new_symbol_names do
    for _, production in ipairs(map_of_productions[i]) do
      new_productions[#new_productions + 1] = (production)
    end
  end

  return new_symbol_names, new_productions
end

---------------------------------------------------------------------------

local first_symbols

-- P.221
local function first_symbol(grammar, symbol, first_table)

  if symbol <= grammar.max_terminal_symbol then
    -- lookahead = -1が存在しているかも
    return { [symbol] = true }
  else

    if first_table then
      return assert(first_table[symbol], "error symbol " .. symbol)
    end

    local productions = grammar.productions
    local first = {}
    for _, body in productions:each(function (v) return v.head == symbol and v.body end) do
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

function first_symbols(grammar, symbols, first_table)
  local first = {}
  for _, symbol in ipairs(symbols) do
    for symbol in pairs(first_symbol(grammar, symbol, first_table)) do
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
  for symbol = 1, grammar.max_nonterminal_symbol do
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
        for j in productions:each(function (v) return v.head == symbol end) do
          items:add(Item(j, 1))
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

  local map_of_to_items = module.map(module.list)

  for _, item in ipairs(items) do
    local index = item.index
    local dot = item.dot
    local symbol = productions[index].body[dot]
    if symbol then
      map_of_to_items[symbol]:add(Item(index, dot + 1))
    end
  end

  local gotos = module.list()
  for symbol, to_items in map_of_to_items:each() do
    lr0_closure(grammar, to_items)
    gotos:add { symbol = symbol, to_items = to_items }
  end

  return gotos
end

-- P.246
local function lr0_items(grammar)
  local start_items = module.list(Item(1, 1))
  lr0_closure(grammar, start_items)
  local set_of_items = module.set(start_items)
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
        transition[data.symbol] = set_of_items:put(data.to_items)
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
      local body = productions[item.index].body
      local first = first_symbols(grammar, module.list(table.unpack(body, item.dot + 1)):add(item.la), first_table)

      local symbol = productions[item.index].body[item.dot]
      for j in productions:each(function (v) return v.head == symbol end) do
        local a = added[j]
        if not a then
          a = {}
          added[j] = a
        end

        -- LA シンボルを期待している
        for la in pairs(first) do
          if not a[la] then
            a[la] = true
            items:add(Item(j, 1, la))
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
    local kernel_items = module.list()
    local kernel_table = module.map()
    for j, item in ipairs(items) do
      local index = item.index
      local dot = item.dot
      if index == 1 or dot > 1 then
        -- カーネル項の生成規則ごとに、dotごとに項の参照をつくる
        kernel_table[index][dot] = j
      end
      -- LALR1ではLAは集合にする
      if index == 1 and dot == 1 then
        kernel_items:add(Item(index, dot, { true }))  -- la = { [marker_end] = true }
      else
        kernel_items:add(Item(index, dot, {}))
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
        -- lookaheadは集合ではない
        local items = module.list():add(Item(from_index, from_dot, -1))  -- la = marker_lookahead
        lr1_closure(grammar, first_table, items)
        for _, item in ipairs(items) do
          local index = item.index
          local production = productions[index]
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
              -- 以降は集合を期待
              set_of_kernel_items[to_i][to_j].la[la] = true
            end
          end
        end
      end
    end
  end

  repeat
    local done = true
    for i = 1, #propagated do
      local op = propagated[i]
      local from_la = set_of_kernel_items[op.from_i][op.from_j].la
      local to_la = set_of_kernel_items[op.to_i][op.to_j].la
      for la in pairs(from_la) do
        if not to_la[la] then
          to_la[la] = true
          done = false
        end
      end
    end
  until done

  local expanded_set_of_kernel_items = {}
  for _, items in ipairs(set_of_kernel_items) do
    local expanded_items = module.list()
    for _, item in ipairs(items) do
      local index = item.index
      local dot = item.dot
      for la in pairs(item.la) do
        expanded_items:add(Item(index, dot, la))
      end
    end
    expanded_set_of_kernel_items[#expanded_set_of_kernel_items + 1] = expanded_items
  end

  return expanded_set_of_kernel_items
end

---------------------------------------------------------------------------

local function build(def)
  local symbol_names = module.list()
  local symbol_map = {}
  for _, v in ipairs(def[1]) do
    symbol_names:add(v)
    symbol_map[v] = #symbol_names
  end
  local max_terminal_symbol = #symbol_names

  for _, v in ipairs(def[2]) do
    if not symbol_map[v[1]] then
      symbol_names:add(v[1])
      symbol_map[v[1]] = #symbol_names
    end
  end

  local productions = module.list()
  for _, v in ipairs(def[2]) do
    local head = assert(symbol_map[v[1]])
    local body = module.list()
    for i = 2, #v do
      body:add(symbol_map[v[i]])
    end
    productions:add(Production(head, body))
  end

  symbol_names[0] = "epsilon"

  return symbol_names, function (name)
    return assert(symbol_map[name])
  end, {
    productions = productions;
    max_terminal_symbol = max_terminal_symbol;
    max_nonterminal_symbol = #symbol_names;
  }
end

---------------------------------------------------------------------------

local symbol_names, _, grammar = build {
  { "a", "b", "c", "d" };
  {
    { "S", "A", "a" };
    { "S", "b" };
    { "A", "A", "c" };
    { "A", "S", "d" };
    { "A" };
  };
}

local new_symbol_names, new_productions = eliminate_left_recursion(grammar, symbol_names)

for _, p in ipairs(new_productions) do
  local body = p.body
  io.write(new_symbol_names[p.head], " ->")
  for j = 1, #body do
    io.write(" ", new_symbol_names[body[j]])
  end
  io.write "\n"
end

print(("="):rep(75))

---------------------------------------------------------------------------

local symbol_names, _, grammar = build {
  { "$", "+", "*", "(", ")", "id" };
  {
    { "E", "T", "E'" };
    { "E'", "+", "T", "E'" };
    { "E'" };
    { "T", "F", "T'" };
    { "T'", "*", "F", "T'" };
    { "T'" };
    { "F", "(", "E", ")" };
    { "F", "id" };
  };
}

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

local symbol_names, _, grammar = build {
  { "$", "c", "d" };
  {
    { "S'", "S" };
    { "S", "C", "C" };
    { "C", "c", "C" };
    { "C", "d" };
  };
}

local first_table = first(grammar)
local start_items = module.list():add(Item(1, 1, 1))
lr1_closure(grammar, first_table, start_items)

for _, item in ipairs(start_items) do
  local dot = item.dot
  local p = grammar.productions[item.index]
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

local symbol_names, _, grammar = build {
  { "$", "+", "*", "(", ")", "id" };
  {
    { "E'", "E" };
    { "E", "E", "+", "T" };
    { "E", "T" };
    { "T", "T", "*", "F" };
    { "T", "F" };
    { "F", "(", "E", ")" };
    { "F", "id" };
  };
}

local el_symbol_names, el_productions = eliminate_left_recursion(grammar, symbol_names)
local el_grammar = {
  productions = el_productions;
  max_terminal_symbol = grammar.max_terminal_symbol;
  max_nonterminal_symbol = #el_symbol_names;
}
local first_table = first(el_grammar)

local items = module.list(Item(1, 1))
lr0_closure(grammar, items)

local items = module.list(Item(1, 2), Item(2, 2))
local gotos = lr0_goto(grammar, items)
for i, data in ipairs(gotos) do
  for _, to_item in ipairs(data.to_items) do
    print(symbol_names[data.symbol], to_item.index, to_item.dot)
  end
end

print(("="):rep(75))

local set_of_items, transitions = lr0_items(grammar)
local set_of_items = lalr1_kernels(grammar, first_table, set_of_items, transitions)
for _, items in ipairs(set_of_items) do
  lr1_closure(grammar, first_table, items)
end

for i, items in ipairs(set_of_items) do
  io.write("I_", i, "\n")
  for _, item in ipairs(items) do
    local dot = item.dot
    local p = grammar.productions[item.index]
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
  -- print(("-"):rep(75))
  for symbol, to in pairs(transitions[i]) do
    io.write("==== ", symbol_names[symbol], " ===> ", "I_", to, "\n")
  end
  print(("-"):rep(75))
end

