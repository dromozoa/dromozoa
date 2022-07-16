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

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.items" }

function class:add(index, dot)
  self[#self + 1] = { index = index, dot = dot }
  return self
end

module.items = setmetatable(class, {
  __call = function ()
    return setmetatable({}, metatable)
  end;
})

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

-- P.246
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

--[[
function class:lr0_goto(items)
  local productions = self.productions
  local symbols = {}
  local map_of_to_items = {}
  for i = 1, #items do
    local item = items[i]
    local id = item.id
    local dot = item.dot
    local symbol = productions[id].body[dot]
    if symbol then
      local to_items = map_of_to_items[symbol]
      if to_items then
        to_items[#to_items + 1] = { id = id, dot = dot + 1 }
      else
        symbols[#symbols + 1] = symbol
        map_of_to_items[symbol] = { { id = id, dot = dot + 1 } }
      end
    end
  end
  local gotos = {}
  for i = 1, #symbols do
    local symbol = symbols[i]
    local to_items = map_of_to_items[symbol]
    self:lr0_closure(to_items)
    gotos[#gotos + 1] = {
      symbol = symbol;
      to_items = to_items;
    }
  end
  return gotos
end
]]

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

--[[
function class:lr0_items()
  local start_items = { { id = 1, dot = 1 } }
  self:lr0_closure(start_items)
  local set_of_items = { start_items }
  local transitions = {}
  local m = 1
  while true do
    local n = #set_of_items
    if m > n then
      break
    end
    for i = m, n do
      local transition = transitions[i]
      if not transition then
        transition = {}
        transitions[i] = transition
      end
      local gotos = self:lr0_goto(set_of_items[i])
      for j = 1, #gotos do
        local data = gotos[j]
        local to_items = data.to_items
        -- to_itemsが空でなければ / to_itemsは空になりえる？
        -- 教科書の条件式に由来
        if to_items[1] then
          local to
          -- set_of_itemsをscanして、to_itemsと同じものがないか探す
          for k = 1, #set_of_items do
            if equal(to_items, set_of_items[k]) then
              to = k
              break
            end
          end
          -- set_of_itemsに含まれていなければ追加
          if not to then
            to = #set_of_items + 1
            set_of_items[to] = to_items
          end
          transition[data.symbol] = to
        end
      end
    end
    m = n + 1
  end
  return set_of_items, transitions
end
]]


-- キーにひもづけられた値がなければ、テーブルを作成して設定する
-- キーにひもづけられた値を返す

-- キーにひもづけられた値がなければ、コンストラクタを実行して設定する
-- キーにひもづけられた値を返す

-- get, access, opt
-- optional
-- conditional

local t = {}
optional_get(t, 1, module.items):add(17, 17)
optional_get(t, 2, module.items):add(42, 42)
optional_get(t, 1, module.items):add(23, 23)

print(t[1][1].index, t[1][2].index, t[2][1].index)

local map = module.map()

map.xyz = 42
map.foo = 69
map.bar = 1
map.baz = 2
map.xyz = 3

for k, v in map:each() do
  print(k, v)
end

-- map = nil
-- print("#", next(private))
-- collectgarbage()
-- collectgarbage()
-- print("#", next(private))


local symbol_names = { "+", "*", "(", ")", "id" }
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
}

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


