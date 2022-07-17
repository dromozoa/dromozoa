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

module.set = setmetatable(class, {
  __call = function (_, ...)
    return setmetatable({...}, metatable)
  end;
})

---------------------------------------------------------------------------

local private = setmetatable({}, { __mode = "k" })
local metatable = { __name = "dromozoa.parser.map" }

local function new(k, v)
  local self = setmetatable({}, metatable)
  private[self] = { set = {} }
  if k and v then
    self[k] = v
  end
  return self
end

function metatable:__newindex(k, v)
  local priv = private[self]
  if not priv.set[k] then
    priv.set[k] = true
    priv[#priv + 1] = k
  end
  rawset(self, k, v)
end

function metatable:__call(k, fn)
  local v = self[k]
  if v then
    return v
  end
  local v = (fn or new)()
  self[k] = v
  return v
end

function metatable:__pairs()
  local priv = private[self]
  local index = 0
  return function (self, key)
    index = index + 1
    local k = priv[index]
    if k then
      return k, self[k]
    end
  end, self
end

module.map = new

---------------------------------------------------------------------------

local class = {}
local metatable = { __index = class, __name = "dromozoa.parser.list" }

local function new(...)
  return setmetatable({...}, metatable)
end

function class:add(...)
  local n = #self
  for i, v in ipairs {...} do
    self[n + i] = v
  end
  return self
end

function class:slice(i, j)
  return new(table.unpack(self, i, j))
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
    return new(...)
  end;
})

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.parser.production" }

module.production = function (head, body)
  return setmetatable({ head = head, body = body }, metatable)
end

---------------------------------------------------------------------------

local metatable = { __name = "dromozoa.parser.item" }

module.item = function (index, dot, la)
  return setmetatable({ index = index, dot = dot, la = la }, metatable)
end

---------------------------------------------------------------------------

local Set = module.set
local Map = module.map
local List = module.list
local Production = module.production
local Item = module.item

---------------------------------------------------------------------------

local MARKER_LOOKAHEAD = -1
local MARKER_EPSILON = 0
local MARKER_END = 1

---------------------------------------------------------------------------

-- P.213
local function eliminate_left_recursion(grammar, symbol_names)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol
  local max_nonterminal_symbol = grammar.max_nonterminal_symbol

  local new_symbol_names = symbol_names:slice()
  local new_productions = List()

  for i = max_terminal_symbol + 1, max_nonterminal_symbol do
    local n = #new_symbol_names + 1
    local n_bodies = List()
    local i_bodies = List()

    for _, body in productions:each(function (v) return v.head == i and v.body end) do
      local symbol = body[1]
      if symbol and symbol > max_terminal_symbol and symbol < i then
        for _, src_body in new_productions:each(function (v) return v.head == symbol and v.body end) do
          local new_body = src_body:slice():add(table.unpack(body, 2))
          if i == new_body[1] then
            n_bodies:add(new_body:slice(2):add(n))
          else
            i_bodies:add(new_body)
          end
        end
      elseif i == body[1] then
        n_bodies:add(body:slice(2):add(n))
      else
        i_bodies:add(body:slice())
      end
    end

    if n_bodies[1] then
      new_symbol_names:add(symbol_names[i] .. "'")
      n_bodies:add(List())
      for _, body in ipairs(i_bodies) do
        body:add(n)
      end
    end

    for _, body in ipairs(i_bodies) do
      new_productions:add(Production(i, body))
    end
    for _, body in ipairs(n_bodies) do
      new_productions:add(Production(n, body))
    end
  end

  return {
    productions = new_productions;
    max_terminal_symbol = max_terminal_symbol;
    max_nonterminal_symbol = #new_symbol_names;
  }, new_symbol_names
end

---------------------------------------------------------------------------

-- P.221
local first_symbols

local function first_symbol(grammar, symbol, first_table)
  if symbol <= grammar.max_terminal_symbol then
    return Map(symbol, true)
  else
    if first_table then
      return first_table[symbol]
    end
    local first = Map()
    for _, body in grammar.productions:each(function (v) return v.head == symbol and v.body end) do
      if body[1] then
        for symbol in pairs(first_symbols(grammar, body, first_table)) do
          first[symbol] = true
        end
      else
        first[MARKER_EPSILON] = true
      end
    end
    return first
  end
end

function first_symbols(grammar, symbols, first_table)
  local first = Map()
  for _, symbol in ipairs(symbols) do
    for symbol in pairs(first_symbol(grammar, symbol, first_table)) do
      first[symbol] = true
    end
    if first[MARKER_EPSILON] then
      first[MARKER_EPSILON] = nil
    else
      return first
    end
  end
  first[MARKER_EPSILON] = true
  return first
end

local function first(grammar)
  local first_table = Map()
  for symbol = grammar.max_terminal_symbol + 1, grammar.max_nonterminal_symbol do
    first_table[symbol] = first_symbol(grammar, symbol)
  end
  return first_table
end

---------------------------------------------------------------------------

-- P.245
local function lr0_closure(grammar, items)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol

  local added = Map()
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

  return items
end

-- P.246
local function lr0_goto(grammar, items)
  local productions = grammar.productions
  local map_of_to_items = Map(List)

  for _, item in ipairs(items) do
    local symbol = productions[item.index].body[item.dot]
    if symbol then
      map_of_to_items(symbol, List):add(Item(item.index, item.dot + 1))
    end
  end

  for _, to_items in pairs(map_of_to_items) do
    lr0_closure(grammar, to_items)
  end

  return map_of_to_items
end

-- P.246
local function lr0_items(grammar)
  local set_of_items = Set(lr0_closure(grammar, List(Item(1, 1))))
  local transitions = Map()

  local m = 1
  while true do
    local n = #set_of_items
    if m > n then
      break
    end
    for i = m, n do
      local items = set_of_items[i]
      local map_of_to_items = lr0_goto(grammar, items)
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

-- P261
local function lr1_closure(grammar, first_table, items)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol

  local added = Map()
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
        local first = first_symbols(grammar, body:slice(item.dot + 1):add(item.la), first_table)
        for j in productions:each(function (v) return v.head == symbol end) do
          for la in pairs(first) do
            if not added(j)[la] then
              items:add(Item(j, 1, la))
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

-- P.272
local function lalr1_kernels(grammar, first_table, set_of_items, transitions)
  local productions = grammar.productions
  local max_terminal_symbol = grammar.max_terminal_symbol

  local set_of_kernel_items = List()
  local map_of_kernel_items = List()

  for _, items in ipairs(set_of_items) do
    local kernel_items = List()
    local kernel_table = Map()
    for i, item in ipairs(items) do
      if item.index == 1 or item.dot > 1 then
        kernel_table(item.index)[item.dot] = i
      end
      if item.index == 1 and item.dot == 1 then
        kernel_items:add(Item(item.index, item.dot, Map(MARKER_END, true)))
      else
        kernel_items:add(Item(item.index, item.dot, Map()))
      end
    end
    set_of_kernel_items:add(kernel_items)
    map_of_kernel_items:add(kernel_table)
  end

  local propagated = List()

  for i, from_items in ipairs(set_of_items) do
    for j, from_item in ipairs(from_items) do
      local from_index = from_item.index
      local from_dot = from_item.dot
      if productions[from_index].head == max_terminal_symbol + 1 or from_dot > 1 then
        local items = List(Item(from_index, from_dot, MARKER_LOOKAHEAD))
        lr1_closure(grammar, first_table, items)
        for _, item in ipairs(items) do
          local symbol = productions[item.index].body[item.dot]
          if symbol then
            local la = item.la
            local to_i = transitions[i][symbol]
            local to_j = map_of_kernel_items[to_i][item.index][item.dot + 1]
            if la == MARKER_LOOKAHEAD then
              propagated:add { from_i = i, from_j = j, to_i = to_i, to_j = to_j }
            else
              set_of_kernel_items[to_i][to_j].la[la] = true
            end
          end
        end
      end
    end
  end

  repeat
    local done = true
    for _, op in ipairs(propagated) do
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

  local expanded_set_of_kernel_items = List()
  for _, items in ipairs(set_of_kernel_items) do
    local expanded_items = List()
    for _, item in ipairs(items) do
      for la in pairs(item.la) do
        expanded_items:add(Item(item.index, item.dot, la))
      end
    end
    expanded_set_of_kernel_items:add(expanded_items)
  end
  return expanded_set_of_kernel_items
end

---------------------------------------------------------------------------

local function build(def)
  local symbol_names = List()
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

  local productions = List()
  for _, v in ipairs(def[2]) do
    local head = assert(symbol_map[v[1]])
    local body = List()
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

local new_grammar, new_symbol_names = eliminate_left_recursion(grammar, symbol_names)

for _, p in ipairs(new_grammar.productions) do
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
local start_items = List():add(Item(1, 1, 1))
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

local el_grammar, el_symbol_names = eliminate_left_recursion(grammar, symbol_names)
local first_table = first(el_grammar)

local items = List(Item(1, 1))
lr0_closure(grammar, items)

local items = List(Item(1, 2), Item(2, 2))
local gotos = lr0_goto(grammar, items)
for symbol, to_items in pairs(gotos) do
  for _, to_item in ipairs(to_items) do
    print(symbol_names[symbol], to_item.index, to_item.dot)
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

