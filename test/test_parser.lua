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


  grammar {
    procutions;
    max_terminal_symbols = 0;
  }

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

-- get, access, opt
-- optional
-- conditional

local t = {}
optional_get(t, 1, module.items):add(17, 17)
optional_get(t, 2, module.items):add(42, 42)
optional_get(t, 1, module.items):add(23, 23)

print(t[1][1].index, t[1][2].index, t[2][1].index)

--[[
local function lr0_items(grammar)
  local start_items = module.items():add(1, 1)
  lr0_closure(grammar, start_items)

  local set_of_items = { start_items }
  local transitions = {}

  local m = 1
  while true do
    local n = #set_of_items
    if m > n then
      break
    end
    for i = m, n do
      local items = set_of_items[i]
      -- local transition = transitions[i]
      -- if not transition then
      --   transition = {}
      --   transitions[i] = transition
      -- end
      -- local gotos = self:lr0_goto(set_of_items[i])

      for symbol in () do
        if lr0_goto(items, symbol) is_not_empty and not_in(set_of_items) then
          set_of_items:add(lr0_goto(items, symbol))
        end
      end




    end
    m = n + 1
  end

end
]]

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

local module = {}

do
  local class = {}
  local metatable = { __index = class, __name = "dromozoa.parser.production" }

  local function new(head, body)
    return setmetatable({ head = head, body = body }, metatable)
  end

  function metatable:__tostring()
    return "{" .. self.head .. "->".. table.concat(self.body, ",") .. "}"
  end

  module.production = setmetatable(class, {
    __call = function (_, ...) return new(...) end;
  })
end

do
  local class = {}
  local metatable = { __index = class, __name = "dromozoa.parser.productions" }

  local function new()
    return setmetatable({}, metatable)
  end

  function class:add(head, body)
    local index = #self + 1
    self[index] = module.production(head, body)
    return index
  end

  function class:each_by_head(head)
    return function (self, index)
      index = index or 0
      for i = index + 1, #self do
        local production = self[i]
        if production.head == head then
          return i, production
        end
      end
    end, self
  end

  module.productions = setmetatable(class, {
    __call = function (_, ...) return new(...) end;
  })
end

do
  local class = {}
  local metatable = { __index = class, __name = "dromozoa.parser.item" }

  local function new(index, dot, la)
    return setmetatable({ index = index, dot = dot, la = la }, metatable)
  end

  module.item = setmetatable(class, {
    __call = function (_, ...) return new(...) end;
  })
end

do
  local class = {}
  local metatable = { __index = class, __name = "dromozoa.parser.items" }

  local function new()
    return setmetatable({}, metatable)
  end

  module.item = setmetatable(class, {
    __call = function (_, ...) return new(...) end;
  })
end

-- local productions = module.productions()
-- productions:add("A", {"b", "c"})
-- productions:add("B", {"c", "d"})
-- productions:add("A", {"c", "d"})


-- P.246
--[[
local function lr0_closure(grammar, items)
  while true do
    for _, item in ipairs(items) do
      -- A -> a . B b
      local B = productions[item.index].body[item.dot]
      if B and B > max_terminal_symbol then
        for i, production in ipairs(productions:find_by_head(B)) do
          -- B -> c
          -- add B -> . c
          items:push { index = i, dot = 1 }
        end
      end
    end
  end

  repeat
    for item in each(J) do
      local A, alpha, B, beta = item
      for production in each(G) do
        -- contraint B == B
        local B, gamma = production
        if B, dot gamma is not_in J then
          add B...
        end
      end
    end
  until no_more_items_are_added_to_J_on_round
end
]]



