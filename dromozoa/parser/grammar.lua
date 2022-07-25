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

local module = {}

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.grammar.list" }

function class:append(...)
  local n = #self
  for i = 1, select("#", ...) do
    self[n + i] = select(i, ...)
  end
  return self
end

function class:slice(i, j)
  return module.list(table.unpack(self, i, j))
end

function module.list(...)
  return setmetatable({...}, metatable)
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.grammar.set" }

local function equal(a, b)
  if a == b then
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

function class:put(v)
  for i, u in ipairs(self) do
    if equal(u, v) then
      return i, u
    end
  end
  local n = #self + 1
  self[n] = v
  return n, v
end

function module.set(...)
  local self = setmetatable({}, metatable)
  for i = 1, select("#", ...) do
    self:put(select(i, ...))
  end
  return self
end

---------------------------------------------------------------------------

local private = setmetatable({}, { __mode = "k" })
local metatable = { __name = "dromozoa.parser.grammar.map" }

function metatable:__newindex(k, v)
  if v ~= nil then
    local priv = private[self]
    local priv_set = priv.set
    if not priv_set[k] then
      priv[#priv + 1] = k
      priv_set[k] = true
    end
  end
  rawset(self, k, v)
end

function metatable:__call(k, fn)
  local v = self[k]
  if v ~= nil then
    return v
  end
  if fn ~= nil then
    v = fn()
  else
    v = module.map()
  end
  self[k] = v
  return v
end

function metatable:__pairs()
  local priv = private[self]
  local index = 0
  return function (self)
    for i = index + 1, #priv do
      local k = priv[i]
      local v = self[k]
      if v ~= nil then
        index = i
        return k, v
      end
    end
  end, self
end

function module.map(...)
  local self = setmetatable({}, metatable)
  private[self] = { set = {} }
  for i = 1, select("#", ...), 2 do
    self[select(i, ...)] = select(i + 1, ...)
  end
  return self
end

---------------------------------------------------------------------------

local timestamp = 0

function module.timestamp()
  timestamp = timestamp + 1
  return timestamp
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.parser.grammar.precedence" }

function metatable:__call(that)
  self[#self + 1] = that
  return self
end

function module.precedence(associativity, ...)
  return setmetatable({ timestamp = module.timestamp(), associativity = associativity, ... }, metatable)
end

function module.left(...)
  return module.precedence("left", ...)
end

function module.right(...)
  return module.precedence("right", ...)
end

function module.nonassoc(...)
  return module.precedence("nonassoc", ...)
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.parser.grammar.bodies" }

function metatable:__bor(that)
  self[#self + 1] = that
  return self
end

function module.bodies(...)
  return setmetatable({ timestamp = module.timestamp(), ... }, metatable)
end

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.grammar.body" }

function class:prec(that)
  self.precedence = that
  return self
end

function metatable:__mod(that)
  self.semantic_action = that
  return self
end

function metatable:__bor(that)
  return module.bodies(self, that)
end

function metatable:__call(that)
  self[#self + 1] = that
  return self
end

function module.body(...)
  return setmetatable({ timestamp = module.timestamp(), ... }, metatable)
end

---------------------------------------------------------------------------

function module.grammar(token_names, that)
  local symbol_names = module.list()
  local symbol_table = module.map()
  for _, name in ipairs(token_names) do
    if symbol_table[name] then
      error("symbol " .. name .. " redefined as a terminal")
    end
    symbol_table[name] = #symbol_names:append(name)
  end
  local max_terminal_symbol = #symbol_names:append "$"

  local data = module.list()
  for k, v in pairs(that) do
    data:append { k = k, v = v }
  end
  table.sort(data, function (a, b) return a.v.timestamp < b.v.timestamp end)

  local augumented_start_head = #symbol_names:append ""
  local augumented_start_body = augumented_start_head + 1

  local productions = module.list { head = augumented_start_head, body = module.list() }
  local precedence = 0
  local precedence_table = module.map()
  local symbol_precedences = module.map()

  for _, u in ipairs(data) do
    local k = u.k
    local v = u.v
    local metaname = getmetatable(v).__name

    if metaname == "dromozoa.parser.grammar.precedence" then
      precedence = precedence + 1
      for _, name in ipairs(v) do
        local symbol = symbol_table[name]
        if symbol and symbol <= max_terminal_symbol then
          symbol_precedences[symbol] = {
            precedence = precedence;
            associativity = v.associativity;
          }
        else
          precedence_table[name] = {
            precedence = precedence;
            associativity = v.associativity;
          }
        end
      end
    else
      if symbol_table[k] then
        error("symbol " .. k .. " redefined as a nonterminal")
      end
      local symbol = #symbol_names:append(k)
      symbol_table[k] = symbol

      if metaname == "dromozoa.parser.grammar.body" then
        productions:append { head = symbol, body = v }
      else
        for _, body in ipairs(v) do
          productions:append { head = symbol, body = body }
        end
      end
    end
  end

  local production_precedences = module.map()
  local semantic_actions = module.map()
  local used_symbols = module.map()
  local used_precedences = module.map()

  for i, production in ipairs(productions) do
    local name = production.body.precedence
    if name then
      local precedence = precedence_table[name]
      if not precedence then
        error("precedence " .. name .. " not defined")
      end
      production_precedences[i] = precedence
      used_precedences[name] = true
    end
    semantic_actions[i] = production.body.semantic_action

    local body = module.list()
    for _, name in ipairs(production.body) do
      local symbol = symbol_table[name];
      if not symbol then
        error("symbol " .. name .. " not defined")
      end
      body:append(symbol)
      used_symbols[symbol] = true
    end
    production.body = body
  end

  productions[1].body:append(augumented_start_body)
  symbol_names[augumented_start_head] = symbol_names[augumented_start_body] .. "'"

  used_symbols[max_terminal_symbol] = true
  used_symbols[augumented_start_head] = true
  used_symbols[augumented_start_body] = true

  for i, v in ipairs(symbol_names) do
    if not used_symbols[i] then
      error("symbol " .. v .. " not used")
    end
  end
  for k in pairs(precedence_table) do
    if not used_precedences[k] then
      error("precedence " .. k .. " not used")
    end
  end

  return {
    symbol_names = symbol_names;
    symbol_table = symbol_table;
    max_terminal_symbol = max_terminal_symbol;
    max_nonterminal_symbol = #symbol_names;
    productions = productions;
    symbol_precedences = symbol_precedences;
    production_precedences = production_precedences;
    semantic_actions = semantic_actions;
  };
end

---------------------------------------------------------------------------

function module.each_production(productions, head)
  return function (productions, index)
    for i = index + 1, #productions do
      local production = productions[i]
      if production.head == head then
        return i, production.body
      end
    end
  end, productions, 0
end

function module.symbol_precedence(grammar, symbol)
  local precedence = grammar.symbol_precedences[symbol]
  if precedence then
    return precedence.precedence, precedence.associativity
  else
    return 0
  end
end

function module.production_precedence(grammar, index)
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
      return module.symbol_precedence(grammar, symbol)
    end
  end
  return 0
end

---------------------------------------------------------------------------

function module.eliminate_left_recursion(grammar)
  local symbol_names = grammar.symbol_names
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol
  local max_nonterminal_symbol = grammar.max_nonterminal_symbol

  local new_symbol_names = symbol_names:slice()
  local new_productions = module.list()

  for i = max_terminal_symbol + 1, max_nonterminal_symbol do
    local n = #new_symbol_names + 1
    local n_bodies = module.list()
    local i_bodies = module.list()

    for _, body in module.each_production(productions, i) do
      local symbol = body[1]
      if symbol and symbol > max_terminal_symbol and symbol < i then
        for _, src_body in module.each_production(new_productions, symbol) do
          local new_body = src_body:slice():append(table.unpack(body, 2))
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
      n_bodies:append(module.list())
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

function module.first_symbol(grammar, symbol)
  if symbol <= grammar.max_terminal_symbol then
    return module.map(symbol, true)
  else
    local first_table = grammar.first_table
    if first_table then
      return first_table[symbol]
    end
    local first = module.map()
    for _, body in module.each_production(grammar.productions, symbol) do
      if body[1] then
        for symbol in pairs(module.first_symbols(grammar, body)) do
          first[symbol] = true
        end
      else
        first[marker_epsilon] = true
      end
    end
    return first
  end
end

function module.first_symbols(grammar, symbols)
  local first = module.map()
  for _, symbol in ipairs(symbols) do
    for symbol in pairs(module.first_symbol(grammar, symbol)) do
      first[symbol] = true
    end
    if first[marker_epsilon] then
      first[marker_epsilon] = nil
    else
      return first
    end
  end
  first[marker_epsilon] = true
  return first
end

function module.first_table(grammar)
  local first_table = module.map()
  for symbol = grammar.max_terminal_symbol + 1, grammar.max_nonterminal_symbol do
    first_table[symbol] = module.first_symbol(grammar, symbol)
  end
  return first_table
end

---------------------------------------------------------------------------

function module.lr0_closure(grammar, items)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol

  local added = module.map()
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
        for j in module.each_production(productions, symbol) do
          items:append { index = j, dot = 1 }
        end
        added[symbol] = true
      end
    end
    m = n + 1
  end

  return items
end

function module.lr0_goto(grammar, items)
  local productions = grammar.productions
  local map_of_to_items = module.map()

  for _, item in ipairs(items) do
    local symbol = productions[item.index].body[item.dot]
    if symbol then
      map_of_to_items(symbol, module.list):append { index = item.index, dot = item.dot + 1 }
    end
  end

  for _, to_items in pairs(map_of_to_items) do
    module.lr0_closure(grammar, to_items)
  end

  return map_of_to_items
end

function module.lr0_items(grammar)
  local set_of_items = module.set(module.lr0_closure(grammar, module.list { index = 1, dot = 1 }))
  local transitions = module.map()

  local m = 1
  while true do
    local n = #set_of_items
    if m > n then
      break
    end
    for i = m, n do
      local items = set_of_items[i]
      local map_of_to_items = module.lr0_goto(grammar, items)
      local transition = transitions(i)
      for symbol, to_items in pairs(map_of_to_items) do
        transition[symbol] = set_of_items:put(to_items)
      end
    end
    m = n + 1
  end

  return set_of_items, transitions
end

---------------------------------------------------------------------------

function module.lr1_closure(grammar, items)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol

  local added = module.map()
  local m = 1
  while true do
    local n = #items
    if m > n then
      break
    end
    for i = m, n do
      local item = items[i]
      local body = productions[item.index].body
      local symbol = body[item.dot]
      if symbol and symbol > max_terminal_symbol then
        local first = module.first_symbols(grammar, body:slice(item.dot + 1):append(item.la))
        for j in module.each_production(productions, symbol) do
          for la in pairs(first) do
            if not added(j)[la] then
              items:append { index = j, dot = 1, la = la }
              added(j)[la] = true
            end
          end
        end
      end
    end
    m = n + 1
  end

  return items
end

---------------------------------------------------------------------------

local marker_lookahead = -1

function module.lalr1_kernels(grammar, set_of_items, transitions)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol

  local set_of_kernel_items = module.list()
  local map_of_kernel_items = module.list()

  for i, items in ipairs(set_of_items) do
    local kernel_items = module.list()
    local kernel_table = module.map()
    for j, item in ipairs(items) do
      if item.index == 1 or item.dot > 1 then
        kernel_table(item.index)[item.dot] = j
      end
      local la = module.map()
      if item.index == 1 and item.dot == 1 then
        la[max_terminal_symbol] = true
      end
      kernel_items:append { index = item.index, dot = item.dot, la = la }
    end
    set_of_kernel_items[i] = kernel_items
    map_of_kernel_items[i] = kernel_table
  end

  local propagations = module.list()

  for from_i, from_items in ipairs(set_of_items) do
    for from_j, from_item in ipairs(from_items) do
      if productions[from_item.index].head == max_terminal_symbol + 1 or from_item.dot > 1 then
        local items = module.list { index = from_item.index, dot = from_item.dot, la = marker_lookahead }
        module.lr1_closure(grammar, items)
        for _, item in ipairs(items) do
          local symbol = productions[item.index].body[item.dot]
          if symbol then
            local to_i = transitions[from_i][symbol]
            local to_j = map_of_kernel_items[to_i][item.index][item.dot + 1]
            if item.la == marker_lookahead then
              propagations:append { from_i = from_i, from_j = from_j, to_i = to_i, to_j = to_j }
            else
              set_of_kernel_items[to_i][to_j].la[item.la] = true
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
      for la in pairs(from_la) do
        if not to_la[la] then
          to_la[la] = true
          done = false
        end
      end
    end
  until done

  local new_set_of_kernel_items = module.list()
  for _, items in ipairs(set_of_kernel_items) do
    local new_items = module.list()
    for _, item in ipairs(items) do
      for la in pairs(item.la) do
        new_items:append { index = item.index, dot = item.dot, la = la }
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
    module.lr1_closure(grammar, items)
  end
  return set_of_items, transitions
end

---------------------------------------------------------------------------

function module.lr1_construct_table(grammar, set_of_items, transitions, fn)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol

  local max_state = #set_of_items
  local actions = module.map()

  for i, items in ipairs(set_of_items) do
    local data = module.map()

    for symbol, j in pairs(transitions[i]) do
      data[symbol] = j
    end

    local error_table = module.map()
    for _, item in ipairs(items) do
      if not productions[item.index].body[item.dot] then
        local buffer = module.list(false, false)

        local action = data[item.la]
        if action then
          if action <= max_state then
            buffer[1] = "shift(" .. action .. ")"
            local precedence, associativity = module.production_precedence(grammar, item.index)
            if precedence > 0 then
              local shift_precedence = module.symbol_precedence(grammar, item.la)
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

  local heads = module.list()
  local sizes = module.list()
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

return setmetatable(module, { __call = function (_, ...) return module.grammar(...) end })
