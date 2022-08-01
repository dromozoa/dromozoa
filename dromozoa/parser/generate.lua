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

local list = require "dromozoa.list"
local tree = require "dromozoa.tree"
local tree_map = require "dromozoa.tree_map"

local module = {}

---------------------------------------------------------------------------
-- eachはO(1)が保証されるけれど、途中でコンテナを変更すると、危険かもしれない
-- pairsはO(log n)が保証されていて、途中でコンテナを変更しても安全
-- ipairsはどうしよう？
--
-- TODO 最終的には、tree_set/tree_mapと名乗る
-- ipairs, pairsは、定義通りの意味をとるべき。
-- index orderとtree orderのふたつのeachを用意する
-- そのほかに、編集安全なeachも用意する
-- indexじゃなくて、handle/pointerと呼ぶ案
-- index/handle/pointerは、deleteが行われるまで使うことができる
-- indexのK,Vはtreeのものをつかえる
--
-- pairsは、handle順を返す
-- eachは、tree順を返す、レンジ指定子もつけられる
--
--
---------------------------------------------------------------------------

local class = {}
local metatable = { __name = "dromozoa.ordered_set" }
local private = setmetatable({}, { __mode = "k" })

-- TODO インターフェースの改良
-- self, handle, insert or updateあたりをかえす
function class:put(k)
  assert(k ~= nil)
  local ok, _, i = private[self]:insert(k)
  return i, ok
end

-- function class:find(k)
--   return private[self]:find(k) ~= nil
-- end

function class:ipairs()
  return ipairs(private[self].K)
end

-- TODO tree_eachを実装する
-- TODO treeインターフェースでもいいが、そうすると、k=>iという順序になって混乱する
-- TODO 比較時には挿入順に意味がないことにするべきなので、k=>nilでただしい。

function metatable:__len()
  error "not supported"
end

-- TODO getにすることも検討
function metatable:__index(k)
  local v = class[k]
  if v ~= nil then
    return v
  end
  if type(k) == "number" then
    local v = private[self].K[k]
    if v ~= nil then
      return v
    end
  end
  error "not supported"
end

function metatable:__newindex()
  error "not supported"
end

function metatable:__pairs()
  error "not supported"
end

local function ordered_set(compare)
  local self = setmetatable({}, metatable)
  private[self] = tree(compare)
  return self
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __name = "dromozoa.ordered_map" }
local private = setmetatable({}, { __mode = "k" })

function class:put(k, v)
  assert(k ~= nil)
  assert(v ~= nil)
  local _, _, i = private[self]:insert(k, v)
  -- TODO 返り値の検討
  return i
end

function class:opt(k, fn)
  local _, v = private[self]:insert(k, nil, fn)
  return v
end

function class:get(k)
  local _, v = private[self]:find(k)
  return v
end

function class:pairs()
  local i = 0
  return function (self)
    i = i + 1
    local k = self.K[i]
    if k == nil then
      return
    else
      return self.K[i], self.V[i]
    end
  end, private[self], nil
end

-- TODO tree_eachを実装する

function metatable:__len()
  error "not supported"
end

function metatable:__index(k)
  -- TODO 数値キーだけを許す？
  local v = class[k]
  if v ~= nil then
    return v
  end
  error "not supported"
end

function metatable:__newindex()
  error "not supported"
end

function metatable:__pairs()
  error "not supported"
end

local function ordered_map(compare)
  local self = setmetatable({}, metatable)
  private[self] = tree(compare)
  return self
end

---------------------------------------------------------------------------

-- TODO これは、binary searchにして、log(n)にしたほうがよい？
-- productionsはそもそも、headでソートされているはず。
-- => とは限らない
-- ordered_setに、いいかんじのcompareをわたしておけばできる
local function each_production(productions, head)
  return function (productions, index)
    for i = index + 1, #productions do
      local production = productions[i]
      if production.head == head then
        return i, production.body
      end
    end
  end, productions, 0
end

---------------------------------------------------------------------------

function module.eliminate_left_recursion(grammar)
  local symbol_names = grammar.symbol_names
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol
  local max_nonterminal_symbol = grammar.max_nonterminal_symbol

  local new_symbol_names = symbol_names:slice()
  local new_productions = list()

  for i = max_terminal_symbol + 1, max_nonterminal_symbol do
    local n = #new_symbol_names + 1
    local n_bodies = list()
    local i_bodies = list()

    for _, body in each_production(productions, i) do
      local symbol = body[1]
      if symbol and symbol > max_terminal_symbol and symbol < i then
        for _, src_body in each_production(new_productions, symbol) do
          local new_body = src_body:slice():append(body:unpack(2))
          if i == new_body[1] then
            n_bodies:append(new_body:slice(2):append(n))
          else
            i_bodies:append(new_body)
          end
        end
      elseif i == body[1] then
        n_bodies:append(body:slice(2):append(n))
      else
        i_bodies:append(body:slice())
      end
    end

    if n_bodies[1] then
      new_symbol_names:append(symbol_names[i] .. "'")
      n_bodies:append(list())
      for _, body in ipairs(i_bodies) do
        body:append(n)
      end
    end

    for _, body in ipairs(i_bodies) do
      new_productions:append { head = i, body = body }
    end
    for _, body in ipairs(n_bodies) do
      new_productions:append { head = n, body = body }
    end
  end

  return {
    symbol_names = new_symbol_names;
    max_terminal_symbol = max_terminal_symbol;
    max_nonterminal_symbol = #new_symbol_names;
    productions = new_productions;
  }
end

---------------------------------------------------------------------------

local marker_epsilon = 0

local first_symbols

local function first_symbol(grammar, symbol)
  if symbol <= grammar.max_terminal_symbol then
    local result = ordered_set()
    result:put(symbol)
    return result
  else
    local first_table = grammar.first_table
    if first_table then
      return first_table[symbol]
    end
    local result = ordered_set()
    local epsilon = false
    for _, body in each_production(grammar.productions, symbol) do
      if body[1] then
        local first = first_symbols(grammar, body)
        for _, symbol in first:ipairs() do
          result:put(symbol)
        end
      else
        result:put(marker_epsilon)
      end
    end
    return result
  end
end

function first_symbols(grammar, symbols)
  local result = ordered_set()
  for _, symbol in ipairs(symbols) do
    local epsilon = false
    for _, symbol in first_symbol(grammar, symbol):ipairs() do
      if symbol == marker_epsilon then
        epsilon = true
      else
        result:put(symbol)
      end
    end
    if not epsilon then
      return result
    end
  end
  result:put(marker_epsilon)
  return result
end

local function first_table(grammar)
  local result = {}
  for symbol = grammar.max_terminal_symbol + 1, grammar.max_nonterminal_symbol do
    result[symbol] = first_symbol(grammar, symbol)
  end
  return result
end

---------------------------------------------------------------------------

local function lr0_closure(grammar, items)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol

  for _, item in items:ipairs() do
    local symbol = productions[item.index].body[item.dot]
    if symbol and symbol > max_terminal_symbol then
      for i in each_production(productions, symbol) do
        items:put { index = i, dot = 1 }
      end
    end
  end

  return items
end

function module.lr0_goto(grammar, items)
  local productions = grammar.productions
  local map_of_to_items = ordered_map()

  for _, item in items:ipairs() do
    local symbol = productions[item.index].body[item.dot]
    if symbol then
      map_of_to_items:opt(symbol, ordered_set):put { index = item.index, dot = item.dot + 1 }
    end
  end
  for _, to_items in map_of_to_items:pairs() do
    lr0_closure(grammar, to_items)
  end

  return map_of_to_items
end

function module.lr0_items(grammar)
  local set_of_items = ordered_set()
  local transitions = {}

  local items = ordered_set()
  items:put { index = 1, dot = 1 }
  lr0_closure(grammar, items)
  set_of_items:put(items)

  for i, items in set_of_items:ipairs() do
    local map_of_to_items = module.lr0_goto(grammar, items)
    local transition = ordered_map()
    for symbol, to_items in map_of_to_items:pairs() do
      transition:put(symbol, set_of_items:put(to_items))
    end
    transitions[i] = transition
  end

  return set_of_items, transitions
end

---------------------------------------------------------------------------

local function lr1_closure(grammar, items)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol

  for _, item in items:ipairs() do
    local body = productions[item.index].body
    local symbol = body[item.dot]
    if symbol and symbol > max_terminal_symbol then
      local first = first_symbols(grammar, body:slice(item.dot + 1):append(item.la))
      for j in each_production(productions, symbol) do
        -- firstに含まれる文字を調べる
        -- TODO epsilonが含まれている？
        -- epsilonが含まれていたら、遷移がどうしようもないのでは？
        for _, la in first:ipairs() do
          items:put { index = j, dot = 1, la = la }
        end
      end
    end
  end

  return items
end

---------------------------------------------------------------------------

local marker_lookahead = -1

function module.lalr1_kernels(grammar, set_of_items, transitions)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol

  local set_of_kernel_items = list()
  local map_of_kernel_items = list()

  for i, items in set_of_items:ipairs() do
    local kernel_items = ordered_set()
    local kernel_table = ordered_map()
    for j, item in items:ipairs() do
      if item.index == 1 or item.dot > 1 then
        -- kernel_table(item.index)[item.dot] = j
        kernel_table:opt(item.index, function () return {} end)[item.dot] = j
      end
      local la = ordered_set()
      if item.index == 1 and item.dot == 1 then
        la:put(max_terminal_symbol)
      end
      local index = kernel_items:put { index = item.index, dot = item.dot, la = la }
      assert(j == index)
    end
    set_of_kernel_items[i] = kernel_items
    map_of_kernel_items[i] = kernel_table
  end

  local propagations = list()

  for from_i, from_items in set_of_items:ipairs() do
    for from_j, from_item in from_items:ipairs() do
      if productions[from_item.index].head == max_terminal_symbol + 1 or from_item.dot > 1 then
        local items = ordered_set()
        items:put { index = from_item.index, dot = from_item.dot, la = marker_lookahead }
        lr1_closure(grammar, items)
        for _, item in items:ipairs() do
          local symbol = productions[item.index].body[item.dot]
          if symbol then
            local to_i = transitions[from_i]:get(symbol)
            -- local to_j = map_of_kernel_items[to_i][item.index][item.dot + 1]
            local to_j = map_of_kernel_items[to_i]:get(item.index)[item.dot + 1]
            if item.la == marker_lookahead then
              propagations:append { from_i = from_i, from_j = from_j, to_i = to_i, to_j = to_j }
            else
              set_of_kernel_items[to_i][to_j].la:put(item.la)
            end
          end
        end
      end
    end
  end

  repeat
    local done = true
    for _, propagation in ipairs(propagations) do
      local from_la = set_of_kernel_items[propagation.from_i][propagation.from_j].la
      local to_la = set_of_kernel_items[propagation.to_i][propagation.to_j].la
      for _, la in from_la:ipairs() do
        local _, inserted = to_la:put(la)
        if inserted then
          done = false
        end
      end
    end
  until done

  local new_set_of_kernel_items = list()
  for _, items in ipairs(set_of_kernel_items) do
    local new_items = ordered_set()
    for _, item in items:ipairs() do
      for _, la in item.la:ipairs() do
        new_items:put { index = item.index, dot = item.dot, la = la }
      end
    end
    new_set_of_kernel_items:append(new_items)
  end
  return new_set_of_kernel_items
end

function module.lalr1_items(grammar)
  local set_of_items, transitions = module.lr0_items(grammar)
  local set_of_items = module.lalr1_kernels(grammar, set_of_items, transitions)
  for _, items in ipairs(set_of_items) do
    lr1_closure(grammar, items)
  end
  return set_of_items, transitions
end

---------------------------------------------------------------------------

local function symbol_precedence(grammar, symbol)
  local precedence = grammar.symbol_precedences[symbol]
  if precedence then
    return precedence.precedence, precedence.associativity
  else
    return 0
  end
end

local function production_precedence(grammar, index)
  local precedence = grammar.production_precedences[index]
  if precedence then
    return precedence.precedence, precedence.associativity
  end

  local max_terminal_symbol = grammar.max_terminal_symbol
  local production = grammar.productions[index]
  local body = production.body
  for i = #body, 1, -1 do
    local symbol = body[i]
    if symbol <= max_terminal_symbol then
      return symbol_precedence(grammar, symbol)
    end
  end
  return 0
end

function module.lr1_construct_table(grammar, set_of_items, transitions, fn)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol

  local max_state = #set_of_items
  local actions = {} -- TODO シークエンスを保証する？

  for i, items in ipairs(set_of_items) do
    local data = {} -- TODO シークエンスを保証する？

    for symbol, j in transitions[i]:pairs() do
      data[symbol] = j
    end

    local error_table = {}
    for _, item in items:ipairs() do
      if not productions[item.index].body[item.dot] then
        local buffer = list(false, false)

        local action = data[item.la]
        if action then
          if action <= max_state then
            buffer[1] = "shift(" .. action .. ")"
            local precedence, associativity = production_precedence(grammar, item.index)
            if precedence > 0 then
              local shift_precedence = symbol_precedence(grammar, item.la)
              if shift_precedence == precedence then
                if associativity == "left" then
                  buffer:append "reduce"
                  data[item.la] = item.index + max_state
                elseif associativity == "nonassoc" then
                  buffer:append "an error"
                  data[item.la] = nil
                  error_table[item.la] = true
                else
                  buffer:append "shift"
                end
                buffer:append(": precedence ", shift_precedence, " == ", precedence, " associativity ", associativity)
              elseif shift_precedence < precedence then
                buffer:append("reduce: precedence ", shift_precedence, " < ", precedence)
                data[item.la] = item.index + max_state
              else
                buffer:append("shift: precedence ", shift_precedence, " > ", precedence)
              end
            else
              buffer:append "shift"
            end
          else
            local index = action - max_state
            buffer[1] = "reduce(" .. index .. ")"
            if item.index < index then
              buffer:append "the latter"
              data[item.la] = item.index + max_state
            else
              buffer:append "the former"
            end
          end
        elseif error_table[item.la] then
          buffer[1] = "error"
          buffer:append "an error"
        else
          data[item.la] = item.index + max_state
        end

        if buffer[1] then
          buffer[2] = " / reduce(" .. item.index .. ") conflict resolved as "
          buffer:append(" at state(", i, ") symbol(", grammar.symbol_names[item.la], ")")
          fn(table.concat(buffer))
        end
      end
    end

    actions[i] = data
  end

  local heads = list()
  local sizes = list()
  for i, production in ipairs(productions) do
    heads[i] = production.head
    sizes[i] = #production.body
  end

  return {
    symbol_names = grammar.symbol_names;
    symbol_table = grammar.symbol_table;
    max_state = max_state;
    max_terminal_symbol = max_terminal_symbol;
    max_nonterminal_symbol = grammar.max_nonterminal_symbol;
    actions = actions;
    heads = heads;
    sizes = sizes;
    semantic_actions = grammar.semantic_actions;
  }
end

---------------------------------------------------------------------------

-- テスト用
module.ordered_set = ordered_set
module.first_symbol = first_symbol
module.first_symbols = first_symbols
module.first_table = first_table
module.lr1_closure = lr1_closure

return module
