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

]]

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

local productions = module.productions()
productions:add("A", {"b", "c"})
productions:add("B", {"c", "d"})
productions:add("A", {"c", "d"})

for index, production in productions:each_by_head "B" do
  print(index, production)
end


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



