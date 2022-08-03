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

local compare = require "dromozoa.compare"
local list = require "dromozoa.list"
local tree_map2 = require "dromozoa.tree_map2"
local tree_set = require "dromozoa.tree_set"

local module = {}

---------------------------------------------------------------------------

local function each_production(productions, head)
  return coroutine.wrap(function (self)
    for i, production in productions:tree_each({ head = head, head_index = 0 }, { head = head + 1, head_index = 0 }) do
      coroutine.yield(i, production.body)
    end
  end), productions
end

---------------------------------------------------------------------------

local function eliminate_left_recursion(grammar)
  local symbol_names = grammar.symbol_names
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol

  local new_symbol_names = symbol_names:slice()
  local new_productions = tree_set(productions.tree_compare)

  for i = max_terminal_symbol + 1, #symbol_names do
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

    for j, body in ipairs(i_bodies) do
      new_productions:insert { head = i, head_index = j, body = body }
    end
    for j, body in ipairs(n_bodies) do
      new_productions:insert { head = n, head_index = j, body = body }
    end
  end

  return {
    symbol_names = new_symbol_names;
    max_terminal_symbol = max_terminal_symbol;
    productions = new_productions;
  }
end

---------------------------------------------------------------------------

local marker_epsilon = 0

local first_symbols

local function first_symbol(grammar, symbol)
  if symbol <= grammar.max_terminal_symbol then
    return tree_set():insert(symbol)
  else
    local first_table = grammar.first_table
    if first_table then
      return first_table[symbol]
    end
    local result = tree_set()
    local epsilon = false
    for _, body in each_production(grammar.productions, symbol) do
      if body[1] then
        local first = first_symbols(grammar, body)
        for _, symbol in first:ipairs() do
          result:insert(symbol)
        end
      else
        result:insert(marker_epsilon)
      end
    end
    return result
  end
end

function first_symbols(grammar, symbols)
  local result = tree_set()
  for _, symbol in ipairs(symbols) do
    local epsilon = false
    for _, symbol in first_symbol(grammar, symbol):ipairs() do
      if symbol == marker_epsilon then
        epsilon = true
      else
        result:insert(symbol)
      end
    end
    if not epsilon then
      return result
    end
  end
  return result:insert(marker_epsilon)
end

local function first_table(grammar)
  local result = {}
  for symbol = grammar.max_terminal_symbol + 1, #grammar.symbol_names do
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
        items:insert { index = i, dot = 1 }
      end
    end
  end

  return items
end

local function lr0_goto(grammar, items)
  local productions = grammar.productions
  local map_of_to_items = tree_map2()

  for _, item in items:ipairs() do
    local symbol = productions[item.index].body[item.dot]
    if symbol then
      map_of_to_items:get(symbol, tree_set):insert { index = item.index, dot = item.dot + 1 }
    end
  end
  for _, to_items in map_of_to_items:pairs() do
    lr0_closure(grammar, to_items)
  end

  return map_of_to_items
end

local function lr0_items(grammar)
  local transitions = {}
  local set_of_items = tree_set():insert(lr0_closure(grammar, tree_set():insert { index = 1, dot = 1 }))

  for i, items in set_of_items:ipairs() do
    local map_of_to_items = lr0_goto(grammar, items)
    local transition = tree_map2()
    for symbol, to_items in map_of_to_items:pairs() do
      transition:insert(symbol, select(2, set_of_items:insert(to_items)))
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
          items:insert { index = j, dot = 1, la = la }
        end
      end
    end
  end

  return items
end

---------------------------------------------------------------------------

local marker_lookahead = -1

local function lalr1_kernels(grammar, set_of_items, transitions)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol

  local set_of_kernel_items = list()
  local map_of_kernel_items = list()

  for i, items in set_of_items:ipairs() do
    local kernel_items = tree_set()
    local kernel_table = tree_map2()
    for j, item in items:ipairs() do
      if item.index == 1 or item.dot > 1 then
        kernel_table:get(item.index, function () return {} end)[item.dot] = j
      end
      local la = tree_set()
      if item.index == 1 and item.dot == 1 then
        la:insert(max_terminal_symbol)
      end
      kernel_items:insert { index = item.index, dot = item.dot, la = la }
    end
    set_of_kernel_items[i] = kernel_items
    map_of_kernel_items[i] = kernel_table
  end

  local propagations = list()

  for from_i, from_items in set_of_items:ipairs() do
    for from_j, from_item in from_items:ipairs() do
      if productions[from_item.index].head == max_terminal_symbol + 1 or from_item.dot > 1 then
        local items = tree_set()
        items:insert { index = from_item.index, dot = from_item.dot, la = marker_lookahead }
        lr1_closure(grammar, items)
        for _, item in items:ipairs() do
          local symbol = productions[item.index].body[item.dot]
          if symbol then
            local to_i = transitions[from_i]:get(symbol)
            local to_j = map_of_kernel_items[to_i]:get(item.index)[item.dot + 1]
            if item.la == marker_lookahead then
              propagations:append { from_i = from_i, from_j = from_j, to_i = to_i, to_j = to_j }
            else
              set_of_kernel_items[to_i][to_j].la:insert(item.la)
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
        if select(3, to_la:insert(la)) then
          done = false
        end
      end
    end
  until done

  local new_set_of_kernel_items = list()
  for _, items in ipairs(set_of_kernel_items) do
    local new_items = tree_set()
    for _, item in items:ipairs() do
      for _, la in item.la:ipairs() do
        new_items:insert { index = item.index, dot = item.dot, la = la }
      end
    end
    new_set_of_kernel_items:append(new_items)
  end
  return new_set_of_kernel_items
end

local function lalr1_items(grammar)
  local set_of_items, transitions = lr0_items(grammar)
  local set_of_items = lalr1_kernels(grammar, set_of_items, transitions)
  for _, items in ipairs(set_of_items) do
    lr1_closure(grammar, items)
  end
  return set_of_items, transitions
end

---------------------------------------------------------------------------

local function symbol_precedence(grammar, symbol)
  local precedence = grammar.symbol_precedences[symbol]
  if precedence ~= nil then
    return precedence.precedence, precedence.associativity
  else
    return 0
  end
end

local function production_precedence(grammar, index)
  local precedence = grammar.production_precedences[index]
  if precedence ~= nil then
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

local function resolve(grammar, max_state, action, item)
  if action == 0 then
    return false, "error"
  end

  if action > max_state then
    local index = action - max_state
    return item.index < index, "reduce(" .. index .. ")"
  end

  local a = "shift(" .. action .. ")"
  local precedence, associativity = production_precedence(grammar, item.index)
  if precedence == 0 then
    return false, a
  end

  local shift_precedence = symbol_precedence(grammar, item.la)
  if shift_precedence < precedence then
    return true, a, ": precedence " .. shift_precedence .. " < " .. precedence
  end
  if shift_precedence > precedence then
    return false, a, ": precedence " .. shift_precedence .. " > " .. precedence
  end

  local message = ": precedence " .. shift_precedence .. " == " .. precedence .. " associativity " .. associativity
  if associativity == "left" then
    return true, a, message
  end
  if associativity == "nonassoc" then
    return nil, a, message
  end
  return false, a, message
end

-- TODO これはcompileにうつす？
local function lr1_construct_table(grammar, set_of_items, transitions, fn)
  assert(fn == nil)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol

  local max_state = #set_of_items
  local actions = {}
  local conflictions = list()

  for i, items in ipairs(set_of_items) do
    local data = {} -- 配列を保証する

    for symbol, j in transitions[i]:pairs() do
      data[symbol] = j
    end

    for _, item in items:ipairs() do
      if productions[item.index].body[item.dot] == nil then
        local action = data[item.la]
        if action == nil then
          data[item.la] = item.index + max_state
        else
          -- shiftとreduceは関数なので()にする
          -- それ以外は？
          local overwrite, a, message = resolve(grammar, max_state, action, item)
          local b = "reduce(" .. item.index .. ")"
          local buffer = list(a, " / ", b, " conflict resolved as ")

          if overwrite == nil then
            data[item.la] = 0
            buffer:append "an error"
          elseif overwrite then
            data[item.la] = item.index + max_state
            buffer:append(b)
          else
            buffer:append(a)
          end
          if message ~= nil then
            buffer:append(message)
          end
          buffer:append(" at state(", i, ") production(", item.index, ") symbol(", grammar.symbol_names[item.la], ")")
          conflictions:append(table.concat(buffer))
        end
      end
    end

    for symbol = 1, #grammar.symbol_names do
      if data[symbol] == nil then
        data[symbol] = 0
      end
    end

    actions[i] = data
  end

  local heads = list()
  local sizes = list()
  for i, production in productions:ipairs() do
    heads[i] = production.head
    sizes[i] = #production.body
  end

  return {
    conflictions = conflictions;
    symbol_names = grammar.symbol_names;
    max_state = max_state;
    max_terminal_symbol = max_terminal_symbol;
    actions = actions;
    heads = heads;
    sizes = sizes;
    semantic_actions = grammar.semantic_actions;
  }
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.parser.parser" }

function metatable:__call(grammar)
  -- TODO テスト用に途中のデータを保存しておく
  local eliminated = eliminate_left_recursion(grammar)
  grammar.first_table = first_table(eliminated)
  local set_of_items, transitions = lalr1_items(grammar)
  local table = lr1_construct_table(grammar, set_of_items, transitions)
  return table
end

---------------------------------------------------------------------------

-- テスト用
module.eliminate_left_recursion = eliminate_left_recursion
module.first_symbol = first_symbol
module.first_symbols = first_symbols
module.first_table = first_table
module.lr0_items = lr0_items
module.lr1_closure = lr1_closure
module.lalr1_kernels = lalr1_kernels
module.lalr1_items = lalr1_items
module.lr1_construct_table = lr1_construct_table

return setmetatable(module, metatable)
