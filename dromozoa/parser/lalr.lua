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

local array = require "dromozoa.array"
local tree_map = require "dromozoa.tree_map"
local tree_set = require "dromozoa.tree_set"

---------------------------------------------------------------------------

local new_timer
pcall(function ()
  new_timer = require "dromozoa.unix".timer
end)

if new_timer == nil then
  local metatable = {
    __index = {
      start = function (self)
        self[1] = os.clock()
        return self
      end;

      stop = function (self)
        self[2] = os.clock()
        return self
      end;

      elapsed = function (self)
        return self[2] - self[1]
      end;
    };
  }

  new_timer = function ()
    return setmetatable({}, metatable)
  end
end

---------------------------------------------------------------------------

local function each_production(productions, head)
  return coroutine.wrap(function (self)
    for i, production in productions:each({ head = head, head_index = 0 }, { head = head + 1, head_index = 0 }) do
      coroutine.yield(i, production.body)
    end
  end), productions
end

---------------------------------------------------------------------------

local function compare_item(a, b)
  if a.index ~= b.index then
    return a.index < b.index and -1 or 1
  end
  if a.dot ~= b.dot then
    return a.dot < b.dot and -1 or 1
  end
  if a.la ~= b.la then
    return a.la < b.la and -1 or 1
  end
  return 0
  -- assert(a.la ~= b.la)
  -- return a.la < b.la and -1 or 1
end

---------------------------------------------------------------------------

local function eliminate_left_recursion(grammar)
  local symbol_names = grammar.symbol_names
  local max_terminal_symbol = grammar.max_terminal_symbol
  local productions = grammar.productions

  local new_symbol_names = symbol_names:slice()
  local new_productions = tree_set(productions.compare)

  for i = max_terminal_symbol + 1, symbol_names:size() do
    local n = new_symbol_names:size() + 1
    local n_bodies = array()
    local i_bodies = array()

    for _, body in each_production(productions, i) do
      local symbol = body:get(1)
      if symbol ~= nil and symbol > max_terminal_symbol and symbol < i then
        for _, src_body in each_production(new_productions, symbol) do
          local new_body = src_body:slice():append(body:unpack(2))
          if i == new_body:get(1) then
            n_bodies:append(new_body:slice(2):append(n))
          else
            i_bodies:append(new_body)
          end
        end
      elseif i == symbol then
        n_bodies:append(body:slice(2):append(n))
      else
        i_bodies:append(body:slice())
      end
    end

    if not n_bodies:empty() then
      new_symbol_names:append(symbol_names:get(i) .. "'")
      n_bodies:append(array())
      for _, body in i_bodies:ipairs() do
        body:append(n)
      end
    end

    for j, body in i_bodies:ipairs() do
      new_productions:insert { head = i, head_index = j, body = body }
    end
    for j, body in n_bodies:ipairs() do
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

-- TODO 途中の情報を保存する
-- 高速化
-- 再帰の検出
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
      if not body:empty() then
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
  for _, symbol in symbols:ipairs() do
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
  for symbol = grammar.max_terminal_symbol + 1, grammar.symbol_names:size() do
    result[symbol] = first_symbol(grammar, symbol)
  end
  return result
end

---------------------------------------------------------------------------

local function lr0_closure(grammar, items)
  local max_terminal_symbol = grammar.max_terminal_symbol
  local productions = grammar.productions

  local added = {}
  for _, item in items:ipairs() do
    local symbol = productions:get(item.index).body:get(item.dot)
    if symbol ~= nil and symbol > max_terminal_symbol and not added[symbol] then
      for i in each_production(productions, symbol) do
        items:insert { index = i, dot = 1 }
      end
      added[symbol] = true
    end
  end

  return items
end

local function lr0_goto(grammar, items)
  local productions = grammar.productions
  local map_of_to_items = tree_map()

  for _, item in items:ipairs() do
    local symbol = productions:get(item.index).body:get(item.dot)
    if symbol ~= nil then
      map_of_to_items:insert_or_update(symbol, function ()
        return tree_set(compare_item):insert { index = item.index, dot = item.dot + 1 }
      end, function (items)
        return items:insert { index = item.index, dot = item.dot + 1 }
      end)
    end
  end
  for _, to_items in map_of_to_items:pairs() do
    lr0_closure(grammar, to_items)
  end

  return map_of_to_items
end

local function lr0_items(grammar)
  local transitions = {}
  local set_of_items = tree_set():insert(lr0_closure(grammar, tree_set(compare_item):insert { index = 1, dot = 1 }))

  for i, items in set_of_items:ipairs() do
    local map_of_to_items = lr0_goto(grammar, items)
    local transition = tree_map()
    for symbol, to_items in map_of_to_items:pairs() do
      transition:assign(symbol, select(2, set_of_items:insert(to_items)))
    end
    transitions[i] = transition
  end

  return set_of_items, transitions
end

---------------------------------------------------------------------------

-- TODO lr1_closure_cache
-- 高速化
local function lr1_closure(grammar, items, timer1, elapsed1)
  local max_terminal_symbol = grammar.max_terminal_symbol
  local productions = grammar.productions
  local lr1_closure_table = grammar.lr1_closure_table

  -- item.index, item.dot, item.la
  local added = {}
  local added2 = {}

  for _, item in items:ipairs() do
    local key = item.index * 1000000 + item.dot * 1000 + item.la + 1
    -- if not added[key] then
    do
      local body = productions:get(item.index).body
      local symbol = body:get(item.dot)
      if symbol ~= nil and symbol > max_terminal_symbol then
        -- firstをキャッシュするのは有効（20-25%程度の改善）
        local first = lr1_closure_table[key]
        if first == nil then
          first = first_symbols(grammar, body:slice(item.dot + 1):append(item.la))
          lr1_closure_table[key] = first
        end

        -- symbolをheadに持つproductionのindex列
        -- firstに含まれるシンボルの集合
        -- firstは、index, dot+1, laに依存
        -- symbolは、index, dotに依存
        -- item.laによってどう変わるか？
        -- body:slice(item.dot + 1)が空でなければ、epsilonは検出されない。
        -- epsilonが検出された場合は、first(item.la)が変える
        -- これが正しければ、item.dot + 1がnilかどうかで判定できる

        -- local first = first_symbols(grammar, body:slice(item.dot + 1):append(item.la))
        if timer1 then timer1:start() end
        for j in each_production(productions, symbol) do
          for _, la in first:ipairs() do
            local key2 = j * 1000 + la + 1
            if not added2[key2] then
              items:insert { index = j, dot = 1, la = la }
              added2[key2] = true
            end
          end
        end
        if timer1 then timer1:stop() elapsed1 = elapsed1 + timer1:elapsed() end

      end
      -- added[key] = true
    end
  end

  return items, elapsed1
end

---------------------------------------------------------------------------

local marker_lookahead = -1

local function lalr1_kernels(grammar, set_of_items, transitions)
  local timer = new_timer()

  local max_terminal_symbol = grammar.max_terminal_symbol
  local productions = grammar.productions

  local set_of_kernel_items = array()
  local map_of_kernel_items = array()

  timer:start()

  for i, items in set_of_items:ipairs() do
    -- TODO kernel_itemsはlaがtree_setなんだけど？
    local kernel_items = tree_set()
    local kernel_table = tree_map()
    for j, item in items:ipairs() do
      if item.index == 1 or item.dot > 1 then
        kernel_table:insert_or_update(item.index, function ()
          return { [item.dot] = j }
        end, function (t)
          t[item.dot] = j
          return t
        end)
      end
      local la = tree_set()
      if item.index == 1 and item.dot == 1 then
        la:insert(max_terminal_symbol)
      end
      kernel_items:insert { index = item.index, dot = item.dot, la = la }
    end
    set_of_kernel_items:append(kernel_items)
    map_of_kernel_items:append(kernel_table)
  end

  timer:stop()
  print("lalr1_kernels A", timer:elapsed())

  local propagations = array()

  timer:start()

  local cache = {}

  local hit = 0
  local miss = 0

  local timer1 = new_timer()
  local timer2 = new_timer()
  local elapsed1 = 0
  local elapsed2 = 0

  grammar.lr1_closure_table = {}

  for from_i, from_items in set_of_items:ipairs() do
    for from_j, from_item in from_items:ipairs() do
      if productions:get(from_item.index).head == max_terminal_symbol + 1 or from_item.dot > 1 then
        -- local items = tree_set()
        -- items:insert { index = from_item.index, dot = from_item.dot, la = marker_lookahead }
        -- print(from_item.index, from_item.dot)

        -- timer1:start()

        local c = cache[from_item.index]
        if not c then
          c = {}
          cache[from_item.index] = c
        end
        local items = c[from_item.dot]
        if not items then
          miss = miss + 1
          items = tree_set(compare_item)
          items:insert { index = from_item.index, dot = from_item.dot, la = marker_lookahead }
          elapsed1 = select(2, lr1_closure(grammar, items, timer1, elapsed1))
          c[from_item.dot] = items
        else
          hit = hit + 1
        end

        -- timer1:stop()
        -- elapsed1 = elapsed1 + timer1:elapsed()

        timer2:start()

        for _, item in items:ipairs() do
          local symbol = productions:get(item.index).body:get(item.dot)
          if symbol ~= nil then
            local to_i = transitions[from_i]:find(symbol)
            local to_j = map_of_kernel_items:get(to_i):find(item.index)[item.dot + 1]
            if item.la == marker_lookahead then
              propagations:append { from_i = from_i, from_j = from_j, to_i = to_i, to_j = to_j }
            else
              set_of_kernel_items:get(to_i):get(to_j).la:insert(item.la)
            end
          end
        end

        timer2:stop()
        elapsed2 = elapsed2 + timer2:elapsed()
      end
    end
  end

  timer:stop()
  print("lalr1_kernels B", timer:elapsed(), hit, miss, elapsed1, elapsed2)

  repeat
    timer:start()

    local done = true
    for _, propagation in propagations:ipairs() do
      local from_la = set_of_kernel_items:get(propagation.from_i):get(propagation.from_j).la
      local to_la = set_of_kernel_items:get(propagation.to_i):get(propagation.to_j).la
      for _, la in from_la:ipairs() do
        if select(3, to_la:insert(la)) then
          done = false
        end
      end
    end

    timer:stop()
    print("lalr1_kernels C", timer:elapsed())
  until done

  timer:start()

  local new_set_of_kernel_items = array()
  for _, items in set_of_kernel_items:ipairs() do
    local new_items = tree_set(compare_item)
    for _, item in items:ipairs() do
      for _, la in item.la:ipairs() do
        new_items:insert { index = item.index, dot = item.dot, la = la }
      end
    end
    new_set_of_kernel_items:append(new_items)
  end

  timer:stop()
  print("lalr1_kernels D", timer:elapsed())

  return new_set_of_kernel_items
end

---------------------------------------------------------------------------

local function symbol_precedence(grammar, symbol)
  local precedence = grammar.symbol_precedences[symbol]
  if precedence ~= nil then
    return precedence.precedence, precedence.name, precedence.associativity
  end
  return 0, grammar.symbol_names:get(symbol)
end

local function production_precedence(grammar, index)
  local production = grammar.productions:get(index)

  local precedence = production.precedence
  if precedence ~= nil then
    return precedence.precedence, precedence.name, precedence.associativity
  end

  local max_terminal_symbol = grammar.max_terminal_symbol
  local body = production.body
  for i = body:size(), 1, -1 do
    local symbol = body:get(i)
    if symbol <= max_terminal_symbol then
      return symbol_precedence(grammar, symbol)
    end
  end

  return 0
end

local function resolve_sr(grammar, item, saction, raction, buffer)
  local rp, rname, associativity = production_precedence(grammar, item.index)
  if rp == 0 then
    return saction
  end
  local sp, sname = symbol_precedence(grammar, item.la)

  if sp < rp then
    return raction, buffer:append("reduce (", sname, " < ", rname, ")")
  elseif rp < sp then
    return saction, buffer:append("shift (", rname, " < ", sname, ")")
  end

  if associativity == "left" then
    return raction, buffer:append("reduce (left ", rname, ")")
  elseif associativity == "right" then
    return saction, buffer:append("shift (right ", rname, ")")
  else
    assert(associativity == "nonassoc")
    return 0, buffer:append("an error (nonassoc ", rname, ")")
  end
end

local function lr1_construct_table(grammar, set_of_items, transitions)
  local symbol_names = grammar.symbol_names
  local expect_sr = grammar.expect_sr
  local productions = grammar.productions

  local max_state = set_of_items:size()
  local actions = {}
  local conflictions = array()
  local total_sr = 0
  local total_rr = 0

  for i, items in set_of_items:ipairs() do
    local sr = 0
    local rr = 0

    local data = {}
    for symbol, j in transitions[i]:pairs() do
      data[symbol] = j
    end

    for _, item in items:ipairs() do
      if productions:get(item.index).body:get(item.dot) == nil then
        local action = data[item.la]
        if action == nil then
          data[item.la] = item.index + max_state
        elseif action > max_state then
          -- reduce/reduce
          if item.index < action - max_state then
            data[item.la] = item.index + max_state
          end
          rr = rr + 1
        elseif action ~= 0 then
          -- shift/reduce
          local buffer = array()
          data[item.la] = resolve_sr(grammar, item, action, item.index + max_state, buffer)
          if buffer:empty() then
            sr = sr + 1
          else
            conflictions:append("[info] conflict between production " .. item.index .. " and symbol " .. symbol_names:get(item.la) .. " resolved as " .. buffer:concat())
          end
        end
      end
    end

    total_sr = total_sr + sr
    total_rr = total_rr + rr
    if sr > 0 or rr > 0 then
      local buffer = array()
      if expect_sr == nil or expect_sr < total_sr or rr > 0 then
        buffer:append "[warn]"
      else
        buffer:append "[info]"
      end
      buffer:append(" state ", i, " conflicts: ")
      if sr > 0 then
        buffer:append(sr, " shift/reduce")
        if rr > 0 then
          buffer:append ", "
        end
      end
      if rr > 0 then
        buffer:append(rr, " reduce/reduce")
      end
      conflictions:append(buffer:concat())
    end

    for symbol in symbol_names:ipairs() do
      if data[symbol] == nil then
        data[symbol] = 0
      end
    end
    actions[i] = data
  end

  if total_sr > 0 then
    local buffer = array()
    if expect_sr ~= total_sr then
      buffer:append "[warn]"
    else
      buffer:append "[info]"
    end
    buffer:append(" shift/reduce conflicts: ", total_sr, " found")
    if expect_sr ~= nil then
      buffer:append(", ", expect_sr, " expected")
    end
    conflictions:append(buffer:concat())
  end
  if total_rr > 0 then
    conflictions:append("[warn] reduce/reduce conflicts: " .. total_rr .. " found")
  end

  return actions, conflictions
end

---------------------------------------------------------------------------

return function (grammar)
  local timer = new_timer()

  timer:start()
  local grammar_without_left_recursion = eliminate_left_recursion(grammar)
  timer:stop()
  print("eliminate_left_recursion", timer:elapsed())

  timer:start()
  grammar.first_table = first_table(grammar_without_left_recursion)
  timer:stop()
  print("first_table", timer:elapsed())

  timer:start()
  local lr0_set_of_items, transitions = lr0_items(grammar)
  timer:stop()
  print("lr0_items", timer:elapsed())

  local lalr1_set_of_items = lalr1_kernels(grammar, lr0_set_of_items, transitions)

  timer:start()
  for i, items in lalr1_set_of_items:ipairs() do
    lr1_closure(grammar, items)
  end
  timer:stop()
  print("lr1_closure", i, timer:elapsed())

  timer:start()
  local actions, conflictions = lr1_construct_table(grammar, lalr1_set_of_items, transitions)
  timer:stop()
  print("lr1_construct_table", timer:elapsed())

  return grammar, actions, conflictions, {
    grammar = grammar;
    grammar_without_left_recursion = grammar_without_left_recursion;
    lr0_set_of_items = lr0_set_of_items;
    lalr1_set_of_items = lalr1_set_of_items;
    transitions = transitions;
  }
end
