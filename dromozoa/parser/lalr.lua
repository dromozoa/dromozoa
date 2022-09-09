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

-- TODO 共通コード
local function append(t, ...)
  local m = #t
  local n = select("#", ...)

  for i = 1, n do
    local v = select(i, ...)
    assert(v ~= nil)
    t[m + i] = v
  end

  return m + n
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

local function eliminate_left_recursion(grammar)
  local symbol_names = grammar.symbol_names
  local max_terminal_symbol = grammar.max_terminal_symbol
  local productions = grammar.productions

  local new_symbol_names = {}
  for i = 1, #symbol_names do
    new_symbol_names[i] = symbol_names[i]
  end
  -- local new_symbol_names = symbol_names:slice()
  local new_productions = tree_set(productions.compare)

  for i = max_terminal_symbol + 1, #symbol_names do
    local n = #new_symbol_names + 1
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
      append(new_symbol_names, symbol_names[i] .. "'")
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

local function first_symbol(grammar, symbol)
  local first_table = grammar.first_table

  local first = first_table[symbol]
  if first == false then
    error "loop detected"
  elseif first ~= nil then
    return first
  end
  first_table[symbol] = false

  if symbol <= grammar.max_terminal_symbol then
    first = tree_set():insert(symbol)
  else
    first = tree_set()
    for _, body in each_production(grammar.productions, symbol) do
      if not body:empty() then
        for _, symbol in first_symbols(grammar, body):ipairs() do
          first:insert(symbol)
        end
      else
        first:insert(marker_epsilon)
      end
    end
  end

  first_table[symbol] = first
  return first
end

function first_symbols(grammar, symbols)
  local first = tree_set()
  for _, symbol in symbols:ipairs() do
    local epsilon = false
    for _, symbol in first_symbol(grammar, symbol):ipairs() do
      if symbol == marker_epsilon then
        epsilon = true
      else
        first:insert(symbol)
      end
    end
    if not epsilon then
      return first
    end
  end
  return first:insert(marker_epsilon)
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
        items:append { index = i, dot = 1 }
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
        return array():append { index = item.index, dot = item.dot + 1 }
      end, function (items)
        return items:append { index = item.index, dot = item.dot + 1 }
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
  local set_of_items = tree_set():insert(lr0_closure(grammar, array():append { index = 1, dot = 1 }))
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

local function lr1_closure(grammar, items)
  local symbol_names = grammar.symbol_names
  local max_terminal_symbol = grammar.max_terminal_symbol
  local productions = grammar.productions
  local lr1_closure_table = grammar.lr1_closure_table

  local skip = {}
  local added = {}

  for _, item in items:ipairs() do
    -- 生成規則の番号 (index) と点の位置 (dot) の組をキーとして使う。
    local item_key = item.index + item.dot * productions:size()

    -- 項 [A -> a . B b, la] を考える。
    -- FIRST(b)がεを含まなければ、FIRST(b) == FIRST(b la)なので、laを考慮する必
    -- 要がない。つまり、laだけが異なる別の項の処理は省略できるので、skip[item_key]
    -- を真に設定する。
    if not skip[item_key] then
      local body = productions:get(item.index).body
      local symbol = body:get(item.dot)
      if symbol ~= nil and symbol > max_terminal_symbol then
        -- FIRST(b)をキャッシュする。
        local first = lr1_closure_table[item_key]
        if first == nil then
          first = first_symbols(grammar, body:slice(item.dot + 1))
          lr1_closure_table[item_key] = first
        end

        -- 点の後の記号 (symbol) とFIRST(b la)に含まれる記号の組をキーとして扱
        -- う。FIRST(b la)は先読み記号 (#) を含む可能性がある。
        local symbol_key = symbol * (#symbol_names + 2)

        for _, la in first:ipairs() do
          if la ~= marker_epsilon then
            if not added[symbol_key + la] then
              for j in each_production(productions, symbol) do
                items:append { index = j, dot = 1, la = la }
              end
              added[symbol_key + la] = true
            end
          end
        end

        if first:find(marker_epsilon) then
          for _, la in first_symbol(grammar, item.la):ipairs() do
            assert(la ~= marker_epsilon)
            if not added[symbol_key + la] then
              for j in each_production(productions, symbol) do
                items:append { index = j, dot = 1, la = la }
              end
              added[symbol_key + la] = true
            end
          end
        else
          skip[item_key] = true
        end
      end
    end
  end

  return items
end

---------------------------------------------------------------------------

local marker_lookahead = -1

local function lalr1_kernels(grammar, set_of_items, transitions)
  local max_terminal_symbol = grammar.max_terminal_symbol
  local productions = grammar.productions

  local set_of_kernel_items = array()
  local map_of_kernel_items = array()

  for i, items in set_of_items:ipairs() do
    local kernel_items = array()
    local kernel_table = {}
    for j, item in items:ipairs() do
      if item.index == 1 or item.dot > 1 then
        local t = kernel_table[item.index]
        if t == nil then
          kernel_table[item.index] = { [item.dot] = j }
        else
          t[item.dot] = j
        end
      end
      local la = tree_set()
      if item.index == 1 and item.dot == 1 then
        la:insert(max_terminal_symbol)
      end
      kernel_items:append { index = item.index, dot = item.dot, la = la }
    end
    set_of_kernel_items:append(kernel_items)
    map_of_kernel_items:append(kernel_table)
  end

  local propagations = array()

  for from_i, from_items in set_of_items:ipairs() do
    for from_j, from_item in from_items:ipairs() do
      if productions:get(from_item.index).head == max_terminal_symbol + 1 or from_item.dot > 1 then
        local items = array():append { index = from_item.index, dot = from_item.dot, la = marker_lookahead }
        lr1_closure(grammar, items)
        for _, item in items:ipairs() do
          local symbol = productions:get(item.index).body:get(item.dot)
          if symbol ~= nil then
            local to_i = transitions[from_i]:find(symbol)
            local to_j = map_of_kernel_items:get(to_i)[item.index][item.dot + 1]
            if item.la == marker_lookahead then
              propagations:append { from_i = from_i, from_j = from_j, to_i = to_i, to_j = to_j }
            else
              set_of_kernel_items:get(to_i):get(to_j).la:insert(item.la)
            end
          end
        end
      end
    end
  end

  repeat
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
  until done

  local new_set_of_kernel_items = array()
  for _, items in set_of_kernel_items:ipairs() do
    local new_items = array()
    for _, item in items:ipairs() do
      for _, la in item.la:ipairs() do
        new_items:append { index = item.index, dot = item.dot, la = la }
      end
    end
    new_set_of_kernel_items:append(new_items)
  end

  return new_set_of_kernel_items
end

---------------------------------------------------------------------------

local function symbol_precedence(grammar, symbol)
  local precedence = grammar.symbol_precedences[symbol]
  if precedence ~= nil then
    return precedence.precedence, precedence.name, precedence.associativity
  end
  return 0, grammar.symbol_names[symbol]
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
            conflictions:append("[info] conflict between production " .. item.index .. " and symbol " .. symbol_names[item.la] .. " resolved as " .. buffer:concat())
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

    for symbol in ipairs(symbol_names) do
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
  -- 左再帰を除去した文法でFIRSTの表を求めて、LR(1)閉包の計算に使う。
  local grammar_without_left_recursion = eliminate_left_recursion(grammar)
  grammar_without_left_recursion.first_table = {}
  -- 元の文法の非終端記号が表に含まれることを保証する。
  for symbol = grammar.max_terminal_symbol + 1, #grammar.symbol_names do
    first_symbol(grammar_without_left_recursion, symbol)
  end
  grammar.first_table = grammar_without_left_recursion.first_table

  local lr0_set_of_items, transitions = lr0_items(grammar)
  grammar.lr1_closure_table = {}
  local lalr1_set_of_items = lalr1_kernels(grammar, lr0_set_of_items, transitions)
  for i, items in lalr1_set_of_items:ipairs() do
    lr1_closure(grammar, items)
  end
  local actions, conflictions = lr1_construct_table(grammar, lalr1_set_of_items, transitions)

  return grammar, actions, conflictions, {
    grammar = grammar;
    grammar_without_left_recursion = grammar_without_left_recursion;
    lr0_set_of_items = lr0_set_of_items;
    lalr1_set_of_items = lalr1_set_of_items;
    transitions = transitions;
  }
end
