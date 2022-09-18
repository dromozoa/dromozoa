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

local append = require "dromozoa.append"
local production_set = require "dromozoa.parser.production_set"
local table_unpack = table.unpack or unpack

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.lalr.symbol_set" }

function class:insert(symbol)
  local map = self.map
  if not map[symbol] then
    local n = #self + 1
    self[n] = symbol
    map[symbol] = n
    return true
  end
end

function class:contains(symbol)
  return self.map[symbol]
end

local function symbol_set(symbol)
  local self = setmetatable({ map = {} }, metatable)
  if symbol then
    self:insert(symbol)
  end
  return self
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
  local new_productions = production_set()

  for i = max_terminal_symbol + 1, #symbol_names do
    local n = #new_symbol_names + 1
    local n_bodies = {}
    local i_bodies = {}

    for _, _, body in productions:each(i) do
      local symbol = body[1]
      if symbol and symbol > max_terminal_symbol and symbol < i then
        for _, _, src_body in new_productions:each(symbol) do
          local new_body = { table_unpack(src_body) }
          append(new_body, table_unpack(body, 2))
          if i == new_body[1] then
            local new_body = { table_unpack(new_body, 2) }
            append(new_body, n)
            append(n_bodies, new_body)
          else
            append(i_bodies, new_body)
          end
        end
      elseif symbol == i then
        local new_body = { table_unpack(body, 2) }
        append(new_body, n)
        append(n_bodies, new_body)
      else
        append(i_bodies, { table_unpack(body) })
      end
    end

    if n_bodies[1] then
      append(new_symbol_names, symbol_names[i] .. "'")
      append(n_bodies, {})
      for _, body in ipairs(i_bodies) do
        append(body, n)
      end
    end

    for _, body in ipairs(i_bodies) do
      new_productions:insert { head = i, body = body }
    end
    for _, body in ipairs(n_bodies) do
      new_productions:insert { head = n, body = body }
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
  elseif first then
    return first
  end
  first_table[symbol] = false

  if symbol <= grammar.max_terminal_symbol then
    first = symbol_set(symbol)
  else
    first = symbol_set()
    for _, _, body in grammar.productions:each(symbol) do
      if body[1] then
        for _, symbol in ipairs(first_symbols(grammar, body)) do
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
  local first = symbol_set()
  for _, symbol in ipairs(symbols) do
    local epsilon = false
    for _, symbol in ipairs(first_symbol(grammar, symbol)) do
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
  first:insert(marker_epsilon)
  return first
end

---------------------------------------------------------------------------

local function lr0_closure(grammar, items)
  local max_terminal_symbol = grammar.max_terminal_symbol
  local productions = grammar.productions

  local added = {}
  for _, item in ipairs(items) do
    local symbol = productions[item.index].body[item.dot]
    if symbol and symbol > max_terminal_symbol and not added[symbol] then
      for _, i in productions:each(symbol) do
        append(items, { index = i, dot = 1 })
      end
      added[symbol] = true
    end
  end

  return items
end

local function lr0_goto(grammar, items)
  local productions = grammar.productions

  local map_of_to_items = {}
  local set_of_to_items = {}

  for _, item in ipairs(items) do
    local symbol = productions[item.index].body[item.dot]
    if symbol then
      local n = map_of_to_items[symbol]
      if n then
        append(set_of_to_items[n], { index = item.index, dot = item.dot + 1 })
      else
        map_of_to_items[symbol] = append(set_of_to_items, { symbol = symbol, { index = item.index, dot = item.dot + 1 } })
      end
    end
  end

  return set_of_to_items
end

local function encode_items(n, items)
  -- 項のリストをカンマ区切りの文字列で表現する。
  local result = {}
  for i, item in ipairs(items) do
    -- 生成規則の番号 (index) と点の位置 (dot) の組で項を表現する。
    result[i] = item.index + item.dot * n
  end
  return table.concat(result, ",")
end

local function lr0_items(grammar)
  local n = #grammar.productions

  local start_items = { { index = 1, dot = 1 } }
  local map_of_items = { [encode_items(n, start_items)] = 1 }
  local set_of_items = { lr0_closure(grammar, start_items) }
  local transitions = {}

  for i, items in ipairs(set_of_items) do
    local transition = {}
    local set_of_to_items = lr0_goto(grammar, items)
    for _, to_items in ipairs(set_of_to_items) do
      local items_key = encode_items(n, to_items)
      local j = map_of_items[items_key]
      if not j then
        j = append(set_of_items, lr0_closure(grammar, to_items))
        map_of_items[items_key] = j
      end
      transition[to_items.symbol] = j
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

  for _, item in ipairs(items) do
    -- 生成規則の番号 (index) と点の位置 (dot) の組をキーとして使う。
    local item_key = item.index + item.dot * #productions

    -- 項 [A -> a . B b, la] を考える。
    -- FIRST(b)がεを含まなければ、FIRST(b) == FIRST(b la)なので、laを考慮する必
    -- 要がない。つまり、laだけが異なる別の項の処理は省略できるので、skip[item_key]
    -- を真に設定する。
    if not skip[item_key] then
      local body = productions[item.index].body
      local symbol = body[item.dot]
      if symbol and symbol > max_terminal_symbol then
        -- FIRST(b)をキャッシュする。
        local first = lr1_closure_table[item_key]
        if not first then
          first = first_symbols(grammar, { table_unpack(body, item.dot + 1) })
          lr1_closure_table[item_key] = first
        end

        -- 点の後の記号 (symbol) とFIRST(b la)に含まれる記号の組をキーとして使
        -- う。FIRST(b la)は先読み記号 (#) を含む可能性がある。
        local symbol_key = symbol * (#symbol_names + 2)

        for _, la in ipairs(first) do
          if la ~= marker_epsilon and not added[symbol_key + la] then
            for _, i in productions:each(symbol) do
              append(items, { index = i, dot = 1, la = la })
            end
            added[symbol_key + la] = true
          end
        end

        if first:contains(marker_epsilon) then
          for _, la in ipairs(first_symbol(grammar, item.la)) do
            assert(la ~= marker_epsilon)
            if not added[symbol_key + la] then
              for _, i in productions:each(symbol) do
                append(items, { index = i, dot = 1, la = la })
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

  local set_of_kernel_items = {}
  local map_of_kernel_items = {}

  for i, items in ipairs(set_of_items) do
    local kernel_items = {}
    local kernel_table = {}
    for j, item in ipairs(items) do
      if item.index == 1 or item.dot > 1 then
        local t = kernel_table[item.index]
        if t then
          t[item.dot] = j
        else
          kernel_table[item.index] = { [item.dot] = j }
        end
      end
      local la = symbol_set()
      if item.index == 1 and item.dot == 1 then
        la:insert(max_terminal_symbol)
      end
      append(kernel_items, { index = item.index, dot = item.dot, la = la })
    end
    set_of_kernel_items[i] = kernel_items
    map_of_kernel_items[i] = kernel_table
  end

  local propagations = {}

  for from_i, from_items in ipairs(set_of_items) do
    for from_j, from_item in ipairs(from_items) do
      if productions[from_item.index].head == max_terminal_symbol + 1 or from_item.dot > 1 then
        local items = { { index = from_item.index, dot = from_item.dot, la = marker_lookahead } }
        lr1_closure(grammar, items)
        for _, item in ipairs(items) do
          local symbol = productions[item.index].body[item.dot]
          if symbol then
            local to_i = transitions[from_i][symbol]
            local to_j = map_of_kernel_items[to_i][item.index][item.dot + 1]
            if item.la == marker_lookahead then
              append(propagations, { from_i = from_i, from_j = from_j, to_i = to_i, to_j = to_j })
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
      for _, la in ipairs(from_la) do
        if to_la:insert(la) then
          done = false
        end
      end
    end
  until done

  local new_set_of_kernel_items = {}
  for _, items in ipairs(set_of_kernel_items) do
    local new_items = {}
    for _, item in ipairs(items) do
      for _, la in ipairs(item.la) do
        append(new_items, { index = item.index, dot = item.dot, la = la })
      end
    end
    append(new_set_of_kernel_items, new_items)
  end

  return new_set_of_kernel_items
end

---------------------------------------------------------------------------

local function symbol_precedence(grammar, symbol)
  local precedence = grammar.symbol_precedences[symbol]
  if precedence then
    return precedence.precedence, precedence.name, precedence.associativity
  end
  return 0, grammar.symbol_names[symbol]
end

local function production_precedence(grammar, index)
  local production = grammar.productions[index]

  local precedence = production.precedence
  if precedence then
    return precedence.precedence, precedence.name, precedence.associativity
  end

  local max_terminal_symbol = grammar.max_terminal_symbol
  local body = production.body
  for i = #body, 1, -1 do
    local symbol = body[i]
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
    append(buffer, "reduce (", sname, " < ", rname, ")")
    return raction, buffer
  elseif rp < sp then
    append(buffer, "shift (", rname, " < ", sname, ")")
    return saction, buffer
  end

  if associativity == "left" then
    append(buffer, "reduce (left ", rname, ")")
    return raction, buffer
  elseif associativity == "right" then
    append(buffer, "shift (right ", rname, ")")
    return saction, buffer
  else
    assert(associativity == "nonassoc")
    append(buffer, "an error (nonassoc ", rname, ")")
    return 0, buffer
  end
end

local function lr1_construct_table(grammar, set_of_items, transitions)
  local symbol_names = grammar.symbol_names
  local expect_sr = grammar.expect_sr
  local productions = grammar.productions

  local max_state = #set_of_items
  local actions = {}
  local conflictions = {}
  local total_sr = 0
  local total_rr = 0

  for i, items in ipairs(set_of_items) do
    local sr = 0
    local rr = 0

    local data = {}
    for symbol, j in pairs(transitions[i]) do
      data[symbol] = j
    end

    for _, item in ipairs(items) do
      if not productions[item.index].body[item.dot] then
        local action = data[item.la]
        if not action then
          data[item.la] = item.index + max_state
        elseif action > max_state then
          -- reduce/reduce
          if item.index < action - max_state then
            data[item.la] = item.index + max_state
          end
          rr = rr + 1
        elseif action ~= 0 then
          -- shift/reduce
          local buffer = {}
          data[item.la] = resolve_sr(grammar, item, action, item.index + max_state, buffer)
          if next(buffer) then
            append(conflictions, "[info] conflict between production " .. item.index .. " and symbol " .. symbol_names[item.la] .. " resolved as " .. table.concat(buffer))
          else
            sr = sr + 1
          end
        end
      end
    end

    total_sr = total_sr + sr
    total_rr = total_rr + rr
    if sr > 0 or rr > 0 then
      local buffer = {}
      if not expect_sr or expect_sr < total_sr or rr > 0 then
        append(buffer, "[warn]")
      else
        append(buffer, "[info]")
      end
      append(buffer, " state ", i, " conflicts: ")
      if sr > 0 then
        append(buffer, sr, " shift/reduce")
        if rr > 0 then
          append(buffer, ", ")
        end
      end
      if rr > 0 then
        append(buffer, rr, " reduce/reduce")
      end
      append(conflictions, table.concat(buffer))
    end

    for symbol in ipairs(symbol_names) do
      if not data[symbol] then
        data[symbol] = 0
      end
    end
    actions[i] = data
  end

  if total_sr > 0 then
    local buffer = {}
    if expect_sr ~= total_sr then
      append(buffer, "[warn]")
    else
      append(buffer, "[info]")
    end
    append(buffer, " shift/reduce conflicts: ", total_sr, " found")
    if expect_sr then
      append(buffer, ", ", expect_sr, " expected")
    end
    append(conflictions, table.concat(buffer))
  end
  if total_rr > 0 then
    append(conflictions, "[warn] reduce/reduce conflicts: " .. total_rr .. " found")
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
  for _, items in ipairs(lalr1_set_of_items) do
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
