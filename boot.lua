-- Copyright (C) 2025 Tomoyuki Fujimori <moyu@dromozoa.com>
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
-- along with dromozoa.  If not, see <https://www.gnu.org/licenses/>.

function quick_sort(t, i, j, compare)
  local n = j - i + 1
  if n <= 1 then
    return
  end

  local pivot = t[i + (n >> 1)]
  local a = i
  local b = j

  while a <= b do
    while __call_indirect1(compare, t[a], pivot) < 0 do
      a = a + 1
    end
    while __call_indirect1(compare, t[b], pivot) > 0 do
      b = b - 1
    end
    if a <= b then
      t[a], t[b] = t[b], t[a]
      a = a + 1
      b = b - 1
    end
  end

  quick_sort(t, i, b, compare)
  quick_sort(t, a, j, compare)
end

function binary_search(t, i, j, compare, v)
  local n = j - i + 1
  while n > 0 do
    local step = n >> 1
    local m = i + step
    local r = __call_indirect1(compare, t[m], v)
    if r == 0 then
      return m
    elseif r < 0 then
      i = m + 1
      n = n - step - 1
    else
      n = step
    end
  end
  return 0
end

--------------------------------------------------------------------------------

--[[
  struct node {
    kind = string;
    attrs = attrs
    union {
      value = string|integer
      items = table<node>
    }
    position = integer?
  }
]]

local attr_class    = 1 -- "token" | "node"
local attr_resolver = 2 -- "fun" | "asm" | "par" | "var" | "call" | "ref" | "set"
local attr_address  = 3 -- 文字列または関数の静的アドレス
local attr_id       = 4 -- 大域ID
local attr_result   = 5 -- 関数の返り値の個数
local attr_global   = 6 -- 大域変数かどうか
local attr_exp      = 7 -- 関数定義または呼び出しが式である
local attr_ref      = 8 -- 参照または変数テーブル

function new_attrs(class)
  return { class, "", 0, 0, -1, false, false, nil }
end

function new_token(kind, value, position)
  return { kind, new_attrs "token", value, position }
end

function new_name(name)
  return new_token("Name", name, 0)
end

function new_node(kind, items)
  return { kind, new_attrs "node", items, 0 }
end

function get_kind(u)
  return u[1]
end

function get_attr(u, key)
  return u[2][key]
end

function set_attr(u, key, value)
  u[2][key] = value
end

function get_value(u)
  if string_compare(get_attr(u, attr_class), "token") == 0 then
    return u[3]
  else
    return nil
  end
end

function get_items(u)
  if string_compare(get_attr(u, attr_class), "node") == 0 then
    return u[3]
  else
    return nil
  end
end

function get_position(u)
  return u[4]
end

function compare_string_index1(a, b)
  return string_compare(a[1], b[1])
end

function compare_string_index2(a, b)
  return string_compare(a[2], b[2])
end

function compare_string_index3(a, b)
  return string_compare(a[3], b[3])
end

--------------------------------------------------------------------------------

local lexer_keywords = nil
local lexer_symbols = nil
local lexer_rules = nil

function lexer_initialize()
  lexer_keywords = {
    "and";
    "break";
    "do";
    "else";
    "elseif";
    "end";
    "false";
    "for";
    "function";
    "if";
    "local";
    "nil";
    "not";
    "or";
    "repeat";
    "return";
    "then";
    "true";
    "until";
    "while";
  }

  lexer_symbols = {
    {
      "#";
      "%";
      "&";
      "(";
      ")";
      "*";
      "+";
      ",";
      "-";
      "/";
      ";";
      "<";
      "=";
      ">";
      "[";
      "]";
      "^";
      "{";
      "|";
      "}";
      "~";
    };
    {
      "..";
      "//";
      "<<";
      "<=";
      "==";
      ">=";
      ">>";
      "~=";
    };
  }

  lexer_rules = {
    lexer_rule_space;
    lexer_rule_comment;
    lexer_rule_keyword_or_name;
    lexer_rule_symbol;
    lexer_rule_string;
    lexer_rule_integer;
  }
end

function lexer_char_to_integer_hex(c, v)
  if 0x30 <= c and c <= 0x39 then
    return true, v * 16 + c - 0x30
  elseif 0x41 <= c and c <= 0x46 then
    return true, v * 16 + c - 0x41 + 10
  elseif 0x61 <= c and c <= 0x66 then
    return true, v * 16 + c - 0x41 + 10
  else
    return false, v
  end
end

function lexer_char_to_integer_dec(c, v)
  if 0x30 <= c and c <= 0x39 then
    return true, v * 10 + c - 0x30
  else
    return false, v
  end
end

function lexer_rule_space(source, position)
  local p = position
  local n = #source

  while p <= n do
    local c = string_byte(source, p)
    if not (0x09 <= c and c <= 0x0D or c == 0x20) then
      break
    end
    p = p + 1
  end

  if p == position then
    return 0, nil
  else
    return p, nil
  end
end

function lexer_rule_comment(source, position)
  local p = position
  local n = #source

  if string_compare(string_sub(source, p, p + 1), "--") ~= 0 then
    return 0, nil
  end
  p = p + 2

  local c = string_byte(source, p)
  if c == 0x5B then -- '['
    local q = p + 1
    local t = { 0x5D } -- ']'

    while q <= n do
      local c = string_byte(source, q)
      if c == 0x5B then -- '['
        q = q + 1
        table_insert(t, 0x5D) -- ']'
        break
      elseif c == 0x3D then -- '='
        q = q + 1
        table_insert(t, 0x3D)
      else
        q = 0
        break
      end
    end

    if q ~= 0 then
      local e = string_char(t)
      while q <= n do
        if string_compare(string_sub(source, q, q + #e - 1), e) == 0 then
          return q + #e, nil
        end
        q = q + 1
      end
      error("lexer error at position "..integer_to_string(q))
    end
  end

  while p <= n do
    local c = string_byte(source, p)
    if c == 0x0A or c == 0x0D then
      break
    end
    p = p + 1
  end

  return p, nil
end

function lexer_rule_keyword_or_name(source, position)
  local p = position
  local n = #source

  local c = string_byte(source, p)
  if not (0x41 <= c and c <= 0x5A or 0x61 <= c and c <= 0x7A or c == 0x5F) then
    return 0, nil
  end
  p = p + 1

  while p <= n do
    local c = string_byte(source, p)
    if not (0x30 <= c and c <= 0x39 or 0x41 <= c and c <= 0x5A or 0x61 <= c and c <= 0x7A or c == 0x5F) then
      break
    end
    p = p + 1
  end

  local v = string_sub(source, position, p - 1)
  local i = binary_search(lexer_keywords, 1, #lexer_keywords, string_compare, v)
  if i ~= 0 then
    return p, new_token(v, v, position)
  end
  return p, new_token("Name", v, position)
end

function lexer_rule_symbol(source, position)
  for i = #lexer_symbols, 1, -1 do
    local symbols = lexer_symbols[i]
    local v = string_sub(source, position, position + i - 1)
    local j = binary_search(symbols, 1, #symbols, string_compare, v)
    if j ~= 0 then
      return position + i, new_token(v, v, position)
    end
  end
  return 0, nil
end

function lexer_rule_string(source, position)
  local p = position
  local n = #source
  local t = {}

  local quote = string_byte(source, p)
  if not (quote == 0x22 or quote == 0x27) then
    return 0, nil
  end
  p = p + 1

  while p <= n do
    local c = string_byte(source, p)
    p = p + 1
    if c == quote then
      return p, new_token("String", string_char(t), position)
    elseif c == 0x5C then
      if p > n then
        error("lexer error at position "..integer_to_string(p))
      end
      local c = string_byte(source, p)
      p = p + 1
      if     c == 0x61 then table_insert(t, 0x07) -- \a
      elseif c == 0x62 then table_insert(t, 0x08) -- \b
      elseif c == 0x74 then table_insert(t, 0x09) -- \t
      elseif c == 0x6E then table_insert(t, 0x0A) -- \n
      elseif c == 0x76 then table_insert(t, 0x0B) -- \v
      elseif c == 0x66 then table_insert(t, 0x0C) -- \f
      elseif c == 0x72 then table_insert(t, 0x0D) -- \r
      elseif c == 0x22 then table_insert(t, 0x22) -- \"
      elseif c == 0x27 then table_insert(t, 0x27) -- \'
      elseif c == 0x5C then table_insert(t, 0x5C) -- \\
      elseif c == 0x78 then -- \xXX
        if p + 1 > n then
          error("lexer error at position "..integer_to_string(p))
        end
        local r = false
        local v = 0
        r, v = lexer_char_to_integer_hex(string_byte(source, p), v)
        if not r then
          error("lexer error at position "..integer_to_string(p))
        end
        p = p + 1
        r, v = lexer_char_to_integer_hex(string_byte(source, p), v)
        if not r then
          error("lexer error at position "..integer_to_string(p))
        end
        p = p + 1
        table_insert(t, v)
      else
        error("lexer error at position "..integer_to_string(p - 1))
      end
    else
      table_insert(t, c)
    end
  end

  error("lexer error at position "..integer_to_string(p))
end

function lexer_rule_integer(source, position)
  local p = position
  local n = #source
  local q = position
  local char_to_integer = lexer_char_to_integer_dec

  local prefix = string_sub(source, p, p + 1)
  if string_compare(prefix, "0X") == 0 or string_compare(prefix, "0x") == 0 then
    p = p + 2
    q = q + 2
    char_to_integer = lexer_char_to_integer_hex
  end

  local r = false
  local v = 0
  while p <= n do
    r, v = __call_indirect2(char_to_integer, string_byte(source, p), v)
    if not r then
      break
    end
    p = p + 1
  end

  if p == q then
    return 0, nil
  else
    return p, new_token("Integer", v, position)
  end
end

function lexer(source)
  lexer_initialize()

  local p = 1
  local n = #source

  local tokens = {}

  while p <= n do
    local q = 0
    local token = nil

    for i = 1, #lexer_rules do
      q, token = __call_indirect2(lexer_rules[i], source, p)
      if q ~= 0 then
        break
      end
    end

    if q == 0 then
      error("lexer error at position "..integer_to_string(p))
    end
    p = q

    if token ~= nil then
      table_insert(tokens, token)
    end
  end

  table_insert(tokens, new_token("EOF", "EOF", p))
  return tokens
end

--------------------------------------------------------------------------------

local parser_nud = nil
local parser_led = nil
local parser_prefix_lbp = 0
local parser_max_lbp = 0

function nud_token(parser, token)
  return token
end

function nud_name(parser, token)
  if string_compare(get_kind(parser_peek(parser)), "String") == 0 then
    local args = new_node("args", { parser_read(parser) })
    local result = new_node("call", { token, args })
    set_attr(result, attr_exp, true)
    return result
  else
    return token
  end
end

function nud_group(parser, token)
  local items = { parser_exp(parser, 0) }
  parser_expect(parser, ")")
  return new_node("group", items)
end

function nud_table(parser, token)
  local items = {}

  while true do
    local node = parser_exp_or_nil(parser)
    if node == nil then
      break
    end
    table_insert(items, node)

    local token = parser_peek(parser)
    if string_compare(get_kind(token), "}") == 0 then
      break
    end

    parser_expect2(parser, ",", ";")
  end
  parser_expect(parser, "}")

  return new_node("table", items)
end

function nud_prefix(parser, token)
  return new_node(get_kind(token), { parser_exp(parser, parser_prefix_lbp) })
end

function led_left(parser, lbp, token, node)
  return new_node(get_kind(token), { node, parser_exp(parser, lbp) })
end

function led_right(parser, lbp, token, node)
  return new_node(get_kind(token), { node, parser_exp(parser, lbp - 1) })
end

function led_call(parser, lbp, token, node)
  local args = parser_list(parser, "args", parser_exp_or_nil, ",", ")")
  local result = new_node("call", { node, args })
  set_attr(result, attr_exp, true)
  return result
end

function led_index(parser, lbp, token, node)
  local items = { node, parser_exp(parser, 0) }
  parser_expect(parser, "]")
  return new_node("index", items)
end

function parser_initialize()
  parser_nud = {}
  parser_led = {}

  table_insert(parser_nud, { "false",   nud_token  })
  table_insert(parser_nud, { "nil",     nud_token  })
  table_insert(parser_nud, { "true",    nud_token  })
  table_insert(parser_nud, { "Name",    nud_name   })
  table_insert(parser_nud, { "String",  nud_token  })
  table_insert(parser_nud, { "Integer", nud_token  })
  table_insert(parser_nud, { "(",       nud_group  })
  table_insert(parser_nud, { "{",       nud_table  })
  table_insert(parser_nud, { "not",     nud_prefix })
  table_insert(parser_nud, { "#",       nud_prefix })
  table_insert(parser_nud, { "-",       nud_prefix })
  table_insert(parser_nud, { "~",       nud_prefix })

  local bp = 10
  table_insert(parser_led, { "or",  bp, led_left   }) bp = bp + 10
  table_insert(parser_led, { "and", bp, led_left   }) bp = bp + 10
  table_insert(parser_led, { "<",   bp, led_left   })
  table_insert(parser_led, { ">",   bp, led_left   })
  table_insert(parser_led, { "<=",  bp, led_left   })
  table_insert(parser_led, { ">=",  bp, led_left   })
  table_insert(parser_led, { "~=",  bp, led_left   })
  table_insert(parser_led, { "==",  bp, led_left   }) bp = bp + 10
  table_insert(parser_led, { "|",   bp, led_left   }) bp = bp + 10
  table_insert(parser_led, { "~",   bp, led_left   }) bp = bp + 10
  table_insert(parser_led, { "&",   bp, led_left   }) bp = bp + 10
  table_insert(parser_led, { "<<",  bp, led_left   })
  table_insert(parser_led, { ">>",  bp, led_left   }) bp = bp + 10
  table_insert(parser_led, { "..",  bp, led_right  }) bp = bp + 10
  table_insert(parser_led, { "+",   bp, led_left   })
  table_insert(parser_led, { "-",   bp, led_left   }) bp = bp + 10
  table_insert(parser_led, { "*",   bp, led_left   })
  table_insert(parser_led, { "/",   bp, led_left   })
  table_insert(parser_led, { "//",  bp, led_left   })
  table_insert(parser_led, { "%",   bp, led_left   }) bp = bp + 10
  parser_prefix_lbp = bp                              bp = bp + 10
  table_insert(parser_led, { "^",   bp, led_right  }) bp = bp + 10
  table_insert(parser_led, { "(",   bp, led_call   })
  table_insert(parser_led, { "[",   bp, led_index  })
  parser_max_lbp = bp

  quick_sort(parser_nud, 1, #parser_nud, compare_string_index1)
  quick_sort(parser_led, 1, #parser_led, compare_string_index1)
end

function parser_error(token)
  error("parser error at token <"..get_kind(token).."> position "..integer_to_string(get_position(token)))
end

function parser_item_search(t, item)
  local i = binary_search(t, 1, #t, compare_string_index1, item)
  if i == 0 then
    return nil
  else
    return t[i]
  end
end

function parser_peek(parser)
  local tokens = parser[1]
  local index = parser[2]
  return tokens[index]
end

function parser_read(parser)
  local tokens = parser[1]
  local index = parser[2]
  local token = tokens[index]
  parser[2] = index + 1
  return token
end

function parser_unread(parser)
  local tokens = parser[1]
  local index = parser[2]
  parser[2] = index - 1
end

function parser_expect(parser, kind)
  local tokens = parser[1]
  local index = parser[2]
  local token = tokens[index]
  if string_compare(get_kind(token), kind) ~= 0 then
    parser_error(token)
  end
  parser[2] = index + 1
  return token
end

function parser_expect2(parser, kind1, kind2)
  local tokens = parser[1]
  local index = parser[2]
  local token = tokens[index]
  local kind = get_kind(token)
  if not (string_compare(kind, kind1) == 0 or string_compare(kind, kind2) == 0) then
    parser_error(token)
  end
  parser[2] = index + 1
  return token
end

function parser_list(parser, kind, parse, separator, close)
  local items = {}

  local i = 0
  while true do
    if i > 0 and separator ~= nil then
      if string_compare(get_kind(parser_peek(parser)), separator) == 0 then
        parser_read(parser)
      else
        break
      end
    end

    local node = __call_indirect1(parse, parser)
    if node == nil then
      if i == 0 or separator == nil then
        break
      else
        parser_error(parser_peek(parser))
      end
    end

    i = i + 1
    table_insert(items, node)
  end

  if close ~= nil then
    parser_expect(parser, close)
  end

  return new_node(kind, items)
end

function parser_block(parser)
  return parser_list(parser, "block", parser_stat, nil, nil)
end

function parser_stat_assign(parser)
  local varlist = parser_list(parser, "varlist", parser_var, ",", "=")
  local explist = parser_list(parser, "explist", parser_exp_or_nil, ",", nil)
  return new_node("assign", { explist, varlist })
end

function parser_stat_if(parser)
  local exp = parser_exp(parser, 0)
  parser_expect(parser, "then")
  local then_block = parser_block(parser)
  local else_block = nil

  local token = parser_read(parser)
  if string_compare(get_kind(token), "elseif") == 0 then
    else_block = new_node("block", { parser_stat_if(parser) })
  elseif string_compare(get_kind(token), "else") == 0 then
    else_block = parser_block(parser)
  else
    parser_unread(parser)
    else_block = new_node("block", {})
  end

  return new_node("if", { exp, then_block, else_block })
end

function parser_stat(parser)
  local token = parser_read(parser)
  if string_compare(get_kind(token), ";") == 0 then
    return new_node(";", {})

  elseif string_compare(get_kind(token), "Name") == 0 then
    -- var ::= Name
    --       | Name       '[' exp ']' { '[' exp ']' }
    --       | Name args  '[' exp ']' { '[' exp ']' }
    --       | '(' exp ') '[' exp ']' { '[' exp ']' }
    -- args ::= '(' ... ')'
    --        | String
    local next_token = parser_peek(parser)
    if string_compare(get_kind(next_token), "(") == 0
      or string_compare(get_kind(next_token), "String") == 0 then
      local index = parser[2]
      parser_read(parser)
      local args = nil
      if string_compare(get_kind(next_token), "(") == 0 then
        args = parser_list(parser, "args", parser_exp_or_nil, ",", ")")
      else
        args = new_node("args", { next_token })
      end
      if string_compare(get_kind(parser_peek(parser)), "[") ~= 0 then
        return new_node("call", { token, args })
      end
      parser[2] = index
    end
    parser_unread(parser)
    return parser_stat_assign(parser)

  elseif string_compare(get_kind(token), "(") == 0 then
    parser_unread(parser)
    return parser_stat_assign(parser)

  elseif string_compare(get_kind(token), "break") == 0 then
    return new_node("break", {})

  elseif string_compare(get_kind(token), "do") == 0 then
    local block = parser_block(parser)
    parser_expect(parser, "end")
    return new_node("do", { block })

  elseif string_compare(get_kind(token), "while") == 0 then
    local exp = parser_exp(parser, 0)
    parser_expect(parser, "do")
    local block = parser_block(parser)
    parser_expect(parser, "end")
    return new_node("while", { exp, block })

  elseif string_compare(get_kind(token), "repeat") == 0 then
    local block = parser_block(parser)
    parser_expect(parser, "until")
    local exp = parser_exp(parser, 0)
    return new_node("repeat", { block, exp })

  elseif string_compare(get_kind(token), "if") == 0 then
    local result = parser_stat_if(parser)
    parser_expect(parser, "end")
    return result

  elseif string_compare(get_kind(token), "for") == 0 then
    local name = parser_expect(parser, "Name")
    parser_expect(parser, "=")
    local exp1 = parser_exp(parser, 0)
    parser_expect(parser, ",")
    local exp2 = parser_exp(parser, 0)
    local exp3 = nil
    if string_compare(get_kind(parser_peek(parser)), ",") == 0 then
      parser_read(parser)
      exp3 = parser_exp(parser, 0)
    else
      exp3 = new_token("Integer", 1, 0)
    end
    parser_expect(parser, "do")
    local block = parser_block(parser)
    parser_expect(parser, "end")
    return new_node("for", { exp1, exp2, exp3, name, block })

  elseif string_compare(get_kind(token), "function") == 0 then
    local name = parser_expect(parser, "Name")
    parser_expect(parser, "(")
    local parlist = parser_list(parser, "parlist", parser_name, ",", ")")
    local block = parser_block(parser)
    parser_expect(parser, "end")
    return new_node("function", { name, parlist, block })

  elseif string_compare(get_kind(token), "local") == 0 then
    -- 初期化無しのローカルは許可しない
    local namelist = parser_list(parser, "namelist", parser_name, ",", "=")
    local explist = parser_list(parser, "explist", parser_exp_or_nil, ",", nil)
    return new_node("local", { explist, namelist })

  elseif string_compare(get_kind(token), "return") == 0 then
    local explist = parser_list(parser, "explist", parser_exp_or_nil, ",", nil)
    return new_node("return", { explist })

  else
    parser_unread(parser)
    return nil
  end
end

function parser_var(parser)
  local node = nil

  local token = parser_read(parser)
  if string_compare(get_kind(token), "Name") == 0 then
    local next_token = parser_peek(parser)
    if string_compare(get_kind(next_token), "(") == 0 then
      parser_read(parser)
      local args = parser_list(parser, "args", parser_exp_or_nil, ",", ")")
      node = new_node("call", { token, args })
      set_attr(node, attr_exp, true)
    elseif string_compare(get_kind(next_token), "String") == 0 then
      parser_read(parser)
      local args = new_node("args", { next_token })
      node = new_node("call", { token, args })
      set_attr(node, attr_exp, true)
    elseif string_compare(get_kind(next_token), "[") ~= 0 then
      return token
    else
      node = token
    end
  elseif string_compare(get_kind(token), "(") then
    node = nud_group(parser, token)
  else
    parser_unread(parser)
    return nil
  end

  repeat
    node = led_index(parser, parser_max_lbp, parser_expect(parser, "["), node)
  until string_compare(get_kind(parser_peek(parser)), "[") ~= 0

  return node
end

function parser_exp_impl(parser, rbp, return_if_nud_is_nil)
  local token = parser_read(parser)
  local nud = parser_item_search(parser_nud, token)
  if nud == nil then
    if return_if_nud_is_nil then
      parser_unread(parser)
      return nil
    else
      parser_error(token)
    end
  end

  local node = __call_indirect1(nud[2], parser, token)
  while true do
    local token = parser_peek(parser)
    local led = parser_item_search(parser_led, token)
    if led == nil or led[2] <= rbp then
      break
    end

    parser_read(parser)
    node = __call_indirect1(led[3], parser, led[2], token, node)
  end
  return node
end

function parser_exp(parser, rbp)
  return parser_exp_impl(parser, rbp, false)
end

function parser_exp_or_nil(parser)
  return parser_exp_impl(parser, 0, true)
end

function parser_name(parser)
  if string_compare(get_kind(parser_peek(parser)), "Name") == 0 then
    return parser_read(parser)
  else
    return nil
  end
end

function parser(tokens)
  parser_initialize()

  local parser = { tokens, 1 }
  local block = parser_block(parser)

  local token = parser_peek(parser)
  if string_compare(get_kind(token), "EOF") ~= 0 then
    parser_error(token)
  end

  return new_node("chunk", { block })
end

--------------------------------------------------------------------------------

function roundup(n, a)
  local r = n % a
  if r == 0 then
    return n
  else
    return n + a - r
  end
end

function make_string_table(tokens)
  local string_tokens = {}

  for i = 1, #tokens do
    local token = tokens[i]
    if string_compare(get_kind(token), "String") == 0 then
      table_insert(string_tokens, token)
    end
  end
  quick_sort(string_tokens, 1, #string_tokens, compare_string_index3)

  local string_table = {}
  local value = nil

  for i = 1, #string_tokens do
    local token = string_tokens[i]
    if value == nil or string_compare(value, get_value(token)) ~= 0 then
      value = get_value(token)
      table_insert(string_table, { value, 0 })
    end
    set_attr(token, attr_address, #string_table * 8)
  end

  local address = (#string_table + 1) * 8
  for i = 1, #string_table do
    local entry = string_table[i]
    entry[2] = address
    address = address + roundup(#entry[1] + 1, 8)
  end

  return string_table, address
end

function encode_i4_hex(v)
  if 0 <= v then
    if v <= 9 then
      return v + 0x30
    elseif v <= 15 then
      return v + 0x41 - 10
    end
  end
  error "out of range"
end

function encode_char(t, v)
  table_insert(t, 0x5C)
  table_insert(t, encode_i4_hex(v >> 4))
  table_insert(t, encode_i4_hex(v & 0xF))
end

function encode_integer(t, v)
  encode_char(t, v & 0xFF)
  encode_char(t, v >> 8 & 0xFF)
  encode_char(t, v >> 16 & 0xFF)
  encode_char(t, v >> 24)
end

function write_string_table(string_table)
  io_write_string '(data 0 (i32.const 8) "'

  for i = 1, #string_table do
    local entry = string_table[i]
    local t = {}
    encode_integer(t, #entry[1])
    encode_integer(t, entry[2])
    io_write_string(string_char(t))
  end

  for i = 1, #string_table do
    local entry = string_table[i]
    local t = {}
    local m = #entry[1]
    local n = roundup(#entry[1] + 1, 8) - m
    for j = 1, m do
      local c = string_byte(entry[1], j)
      if 0 <= c and c <= 0x1F or c == 0x22 or c == 0x5C or c == 0x7F then
        encode_char(t, c)
      else
        table_insert(t, c)
      end
    end
    for j = 1, n do
      encode_char(t, 0)
    end
    io_write_string(string_char(t))
  end

  io_write_string '")\n'
end

function new_ctx()
  return { 0, 0 }
end

local ctx_id = 1
local ctx_address = 2
local ctx_length = 3
local ctx_concat = 4
local ctx_power = 5
local ctx_new_table = 6
local ctx_set_table = 7
local ctx_get_table = 8

function make_id(ctx)
  local id = ctx[ctx_id] + 1
  ctx[ctx_id] = id
  return id
end

function make_address(ctx)
  local address = ctx[ctx_address] + 1
  ctx[ctx_address] = address
  return address
end

function process1(ctx, proto_table, proto, u, v)
  local kind = get_kind(v)
  local items = get_items(v)

  if string_compare(kind, "function") == 0 then
    if proto ~= nil then
      error "compiler error: invalid proto"
    end
    proto = items[1]
    add_fun(ctx, proto_table, proto, -1)

  elseif string_compare(kind, "return") == 0 then
    local result = #get_items(items[1])
    if get_attr(proto, attr_result) == -1 then
      set_attr(proto, attr_result, result)
    elseif get_attr(proto, attr_result) ~= result then
      error "compiler error: invalid result"
    end
  end

  if items ~= nil then
    for i = 1, #items do
      process1(ctx, proto_table, proto, v, items[i])
    end
  end

  if string_compare(kind, "function") == 0 then
    if get_attr(proto, attr_result) == -1 then
      set_attr(proto, attr_result, 0)
    end
  end
end

function new_scope(parent)
  return { {}, parent }
end

local scope_data = 1
local scope_parent = 2

function add_var_impl(ctx, var_table, scope, u, resolver, global)
  local id = make_id(ctx)
  set_attr(u, attr_resolver, resolver)
  set_attr(u, attr_id, id)
  set_attr(u, attr_global, global)
  table_insert(var_table, u)
  table_insert(scope[scope_data], u)
  return id
end

function add_global(ctx, var_table, scope, u)
  return add_var_impl(ctx, var_table, scope, u, "var", true)
end

function add_var(ctx, var_table, scope, u)
  return add_var_impl(ctx, var_table, scope, u, "var", false)
end

function add_par(ctx, var_table, scope, u)
  return add_var_impl(ctx, var_table, scope, u, "par", false)
end

function add_fun(ctx, proto_table, u, result)
  table_insert(proto_table, u)
  local id = make_id(ctx)
  set_attr(u, attr_resolver, "fun")
  set_attr(u, attr_address, make_address(ctx))
  set_attr(u, attr_id, id)
  set_attr(u, attr_result, result)
  set_attr(u, attr_ref, {})
  return id
end

function add_asm(ctx, proto_table, u, result)
  table_insert(proto_table, u)
  set_attr(u, attr_resolver, "asm")
  set_attr(u, attr_result, result)
end

function resolve_name_impl(proto_table, scope, u, resolver)
  while scope ~= nil do
    local data = scope[scope_data]
    for i = #data, 1, -1 do
      local v = data[i]
      if string_compare(get_value(u), get_value(v)) == 0 then
        set_attr(u, attr_resolver, resolver)
        set_attr(u, attr_ref, v)
        return v
      end
    end
    scope = scope[scope_parent]
  end

  for i = 1, #proto_table do
    local v = proto_table[i]
    if string_compare(get_value(u), get_value(v)) == 0 then
      set_attr(u, attr_resolver, resolver)
      set_attr(u, attr_ref, v)
      return v
    end
  end

  error("compiler error: cannot resolve <"..get_value(u)..">")
end

function resolve_name(proto_table, scope, u)
  return resolve_name_impl(proto_table, scope, u, "ref")
end

function resolve_call(proto_table, scope, u)
  return resolve_name_impl(proto_table, scope, u, "call")
end

function new_loop(ctx, u)
  local loop = { make_id(ctx), make_id(ctx) }
  set_attr(u, attr_ref, loop)
  return loop
end

local loop_block = 1
local loop_loop = 2

function process2(ctx, proto_table, var_table, proto, scope, loop, u, v)
  local kind = get_kind(v)
  local items = get_items(v)

  if string_compare(kind, "block") == 0 then
    -- then blockとelse blockにスコープを割り当てる
    if string_compare(get_kind(u), "if") == 0 then
      scope = new_scope(scope)
    end

  elseif string_compare(kind, "assign") == 0 then
    local varlist = get_items(items[2])
    for i = 1, #varlist do
      local var = varlist[i]
      if string_compare(get_kind(var), "Name") == 0 then
        resolve_name(proto_table, scope, var)
      end
      set_attr(var, attr_resolver, "set")
    end

  elseif string_compare(kind, "break") == 0 then
    if loop == nil then
      error "compiler error: invalid loop"
    end
    set_attr(v, attr_id, loop[loop_block])

  elseif string_compare(kind, "do") == 0 then
    scope = new_scope(scope)

  elseif string_compare(kind, "while") == 0 then
    loop = new_loop(ctx, v)
    scope = new_scope(scope)

  elseif string_compare(kind, "repeat") == 0 then
    loop = new_loop(ctx, v)
    scope = new_scope(scope)

  elseif string_compare(kind, "for") == 0 then
    loop = new_loop(ctx, v)
    scope = new_scope(scope)

    set_attr(v, attr_id, add_var(ctx, var_table, scope, new_name "(var)"))
    add_var(ctx, var_table, scope, new_name "(limit)")
    add_var(ctx, var_table, scope, new_name "(step)")

    add_var(ctx, var_table, scope, items[4])

  elseif string_compare(kind, "function") == 0 then
    proto = items[1]
    var_table = get_attr(proto, attr_ref)
    scope = new_scope(scope)

  elseif string_compare(kind, "Name") == 0 then
    if string_compare(get_kind(u), "namelist") == 0 then
      if proto == nil then
        add_global(ctx, var_table, scope, v)
      else
        add_var(ctx, var_table, scope, v)
      end
    elseif string_compare(get_kind(u), "parlist") == 0 then
      add_par(ctx, var_table, scope, v)
    end

    if string_compare(get_attr(v, attr_resolver), "") == 0 then
      if string_compare(get_kind(u), "call") == 0 then
        resolve_call(proto_table, scope, v)
      else
        resolve_name(proto_table, scope, v)
      end
    end

  elseif string_compare(kind, "table") == 0 then
    set_attr(v, attr_id, add_var(ctx, var_table, scope, new_name "(table)"))
  end

  if items ~= nil then
    for i = 1, #items do
      process2(ctx, proto_table, var_table, proto, scope, loop, v, items[i])
    end
  end
end

function process3(ctx, proto, u, v)
  local kind = get_kind(v)
  local items = get_items(v)

  local range_i = 1
  local range_j = 0
  if items ~= nil then
    range_j = #items
  end

  if string_compare(kind, "assign") == 0 then
    range_j = 1

  elseif string_compare(kind, "call") == 0 then
    if string_compare(get_kind(items[1]), "Name") ~= 0 then
      error "compiler error: invalid name"
    end

    range_i = 2

    local ref = get_attr(items[1], attr_ref)
    if string_compare(get_attr(ref, attr_resolver), "asm") == 0 then
      local name = get_value(ref)
      if string_compare(name, "__call_indirect0") == 0
        or string_compare(name, "__call_indirect1") == 0
        or string_compare(name, "__call_indirect2") == 0
        or string_compare(name, "__call_indirect3") == 0
        or string_compare(name, "__export_start") == 0 then
        range_j = 0
      end
    end

  elseif string_compare(kind, "if") == 0 then
    range_j = 1

  elseif string_compare(kind, "while") == 0 then
    range_i = 2

    local loop = get_attr(v, attr_ref)
    io_write_string "block $"
    io_write_integer(loop[loop_block])
    io_write_string "\n"
    io_write_string "loop $"
    io_write_integer(loop[loop_loop])
    io_write_string "\n"

    process3(ctx, proto, v, items[1])

    io_write_string "(i32.eqz)\n"
    io_write_string "(br_if $"
    io_write_integer(loop[loop_block])
    io_write_string ")\n"

  elseif string_compare(kind, "repeat") == 0 then
    local loop = get_attr(v, attr_ref)
    io_write_string "block $"
    io_write_integer(loop[loop_block])
    io_write_string "\n"
    io_write_string "loop $"
    io_write_integer(loop[loop_loop])
    io_write_string "\n"

  elseif string_compare(kind, "for") == 0 then
    range_i = 5

    local loop = get_attr(v, attr_ref)
    io_write_string "block $"
    io_write_integer(loop[loop_block])
    io_write_string "\n"

    local var = get_attr(v, attr_id)

    process3(ctx, proto, v, items[1])
    process3(ctx, proto, v, items[2])
    process3(ctx, proto, v, items[3])

    io_write_string "(local.set $"
    io_write_integer(var + 2)
    io_write_string ")\n"

    io_write_string "(local.set $"
    io_write_integer(var + 1)
    io_write_string ")\n"

    io_write_string "(local.set $"
    io_write_integer(var)
    io_write_string ")\n"

    io_write_string "(local.get $"
    io_write_integer(var)
    io_write_string ")\n"

    io_write_string "(local.get $"
    io_write_integer(var + 2)
    io_write_string ")\n"

    io_write_string "(i32.sub)\n"

    io_write_string "(local.set $"
    io_write_integer(var)
    io_write_string ")\n"

    io_write_string "loop $"
    io_write_integer(loop[loop_loop])
    io_write_string "\n"

    io_write_string "(local.get $"
    io_write_integer(var)
    io_write_string ")\n"

    io_write_string "(local.get $"
    io_write_integer(var + 2)
    io_write_string ")\n"

    io_write_string "(i32.add)\n"

    io_write_string "(local.set $"
    io_write_integer(var)
    io_write_string ")\n"

    io_write_string "(local.get $"
    io_write_integer(var + 2)
    io_write_string ")\n"

    io_write_string "(i32.const 0)\n"

    io_write_string "(i32.ge_s)\n"

    io_write_string "if\n"

      io_write_string "(local.get $"
      io_write_integer(var)
      io_write_string ")\n"

      io_write_string "(local.get $"
      io_write_integer(var + 1)
      io_write_string ")\n"

      io_write_string "(i32.gt_s)\n"

      io_write_string "(br_if $"
      io_write_integer(loop[loop_block])
      io_write_string ")\n"

    io_write_string "else\n"

      io_write_string "(local.get $"
      io_write_integer(var)
      io_write_string ")\n"

      io_write_string "(local.get $"
      io_write_integer(var + 1)
      io_write_string ")\n"

      io_write_string "(i32.lt_s)\n"

      io_write_string "(br_if $"
      io_write_integer(loop[loop_block])
      io_write_string ")\n"

    io_write_string "end\n"

    io_write_string "(local.get $"
    io_write_integer(var)
    io_write_string ")\n"

    io_write_string "(local.set $"
    io_write_integer(get_attr(items[4], attr_id))
    io_write_string ")\n"

  elseif string_compare(kind, "function") == 0 then
    range_i = 3

    proto = items[1]
    io_write_string "(func $"
    io_write_integer(get_attr(proto, attr_id))
    io_write_string " (; "
    io_write_string(get_value(proto))
    io_write_string " ;)\n"

    local parlist = get_items(items[2])
    for i = 1, #parlist do
      local par = parlist[i]
      io_write_string "(param $"
      io_write_integer(get_attr(par, attr_id))
      io_write_string " i32) (; "
      io_write_string(get_value(par))
      io_write_string " ;)\n"
    end

    local result = get_attr(proto, attr_result)
    if result > 0 then
      io_write_string "(result"
      for i = 1, result do
        io_write_string " i32"
      end
      io_write_string ")\n"
    end

    local var_table = get_attr(proto, attr_ref)
    for i = 1, #var_table do
      local var = var_table[i]
      if string_compare(get_attr(var, attr_resolver), "var") == 0 then
        io_write_string "(local $"
        io_write_integer(get_attr(var, attr_id))
        io_write_string " i32) (; "
        io_write_string(get_value(var))
        io_write_string " ;)\n"
      end
    end

    for i = 1, result do
      io_write_string "(local $r"
      io_write_integer(i)
      io_write_string " i32)\n"
    end
    io_write_string "(local $dup i32)\n"

    io_write_string "block $main\n"

  elseif string_compare(kind, "local") == 0 then
    if proto == nil then
      range_j = 0

      local explist = get_items(items[1])
      local namelist = get_items(items[2])
      if not (#explist == #namelist) then
        error "compiler error: invalid global"
      end

      for i = 1, #explist do
        local exp = explist[i]
        local name = namelist[i]

        io_write_string "(global $"
        io_write_integer(get_attr(name, attr_id))
        io_write_string " (mut i32)\n"
        process3(ctx, proto, explist, exp)
        io_write_string ")\n"
      end
    else
      range_j = 1
    end

  elseif string_compare(kind, "false") == 0 then
    io_write_string "(i32.const 0) (; false ;)\n"

  elseif string_compare(kind, "nil") == 0 then
    io_write_string "(i32.const 0) (; nil ;)\n"

  elseif string_compare(kind, "true") == 0 then
    io_write_string "(i32.const 1) (; true ;)\n"

  elseif string_compare(kind, "Name") == 0 then
    if string_compare(get_attr(v, attr_resolver), "ref") == 0 then
      local ref = get_attr(v, attr_ref)
      if string_compare(get_attr(ref, attr_resolver), "fun") == 0 then
        io_write_string "(i32.const "
        io_write_integer(get_attr(ref, attr_address))
        io_write_string ") (; "
      else
        if get_attr(ref, attr_global) then
          io_write_string "(global.get $"
        else
          io_write_string "(local.get $"
        end
        io_write_integer(get_attr(ref, attr_id))
        io_write_string ") (; "
      end
      io_write_string(get_value(v))
      io_write_string " ;)\n"
    end

  elseif string_compare(kind, "Integer") == 0 then
    io_write_string "(i32.const "
    io_write_integer(get_value(v))
    io_write_string ") (; Integer ;)\n"

  elseif string_compare(kind, "String") == 0 then
    io_write_string "(i32.const "
    io_write_integer(get_attr(v, attr_address))
    io_write_string ") (; String ;)\n"

  elseif string_compare(kind, "table") == 0 then
    io_write_string "(i32.const "
    io_write_integer(#get_items(v))
    io_write_string ")\n"
    io_write_string "(call $"
    io_write_integer(get_attr(ctx[ctx_new_table], attr_id))
    io_write_string ") (; __new_table ;)\n"
    io_write_string "(local.tee $"
    io_write_integer(get_attr(v, attr_id))
    io_write_string ")\n"

  elseif string_compare(kind, "or") == 0 then
    range_i = 2

    process3(ctx, proto, v, items[1])
    io_write_string "(local.tee $dup)\n"
    io_write_string "if (result i32)\n"
    io_write_string "(local.get $dup)\n"
    io_write_string "else\n"

  elseif string_compare(kind, "and") == 0 then
    range_i = 2

    process3(ctx, proto, v, items[1])
    io_write_string "(local.tee $dup)\n"
    io_write_string "if (result i32)\n"

  elseif string_compare(kind, "-") == 0 then
    if #get_items(v) == 1 then
      io_write_string "(i32.const 0)\n"
    end
  end

  if items ~= nil then
    for i = range_i, range_j do
      process3(ctx, proto, v, items[i])
    end
  end

  if string_compare(kind, "assign") == 0 then
    local varlist = get_items(items[2])
    for i = #varlist, 1, -1 do
      local var = varlist[i]
      if string_compare(get_kind(var), "Name") == 0 then
        local ref = get_attr(var, attr_ref)
        if get_attr(ref, attr_global) then
          io_write_string "(global.set $"
        else
          io_write_string "(local.set $"
        end
        io_write_integer(get_attr(ref, attr_id))
        io_write_string ") (; "
        io_write_string(get_value(var))
        io_write_string " ;)\n"
      elseif string_compare(get_kind(var), "index") == 0 then
        process3(ctx, proto, varlist, var)
        io_write_string "(call $"
        io_write_integer(get_attr(ctx[ctx_set_table], attr_id))
        io_write_string ") (; __set_table ;)\n"
      else
        error("compiler error: invalid assign <"..get_kind(var)..">")
      end
    end

  elseif string_compare(kind, "call") == 0 then
    local ref = get_attr(items[1], attr_ref)
    local result = get_attr(ref, attr_result)

    if string_compare(get_attr(ref, attr_resolver), "asm") == 0 then
      local name = get_value(ref)
      if string_compare(name, "__call_indirect0") == 0
        or string_compare(name, "__call_indirect1") == 0
        or string_compare(name, "__call_indirect2") == 0
        or string_compare(name, "__call_indirect3") == 0 then

        local args = get_items(items[2])
        for i = 2, #args do
          process3(ctx, proto, items[2], args[i])
        end
        process3(ctx, proto, items[2], args[1])

        io_write_string "(call_indirect"
        if #args > 1 then
          io_write_string " (param"
          for i = 2, #args do
            io_write_string " i32"
          end
          io_write_string ")"
        end

        if result > 0 then
          io_write_string " (result"
          for i = 1, result do
            io_write_string " i32"
          end
          io_write_string ")"
        end

        io_write_string ")\n"

      elseif string_compare(name, "__i32_load") == 0 then
        io_write_string "(i32.load)\n"

      elseif string_compare(name, "__i32_load8") == 0 then
        io_write_string "(i32.load8_u)\n"

      elseif string_compare(name, "__i32_store") == 0 then
        io_write_string "(i32.store)\n"

      elseif string_compare(name, "__i32_store8") == 0 then
        io_write_string "(i32.store8)\n"

      elseif string_compare(name, "__i32_clz") == 0 then
        io_write_string "(i32.clz)\n"

      elseif string_compare(name, "__i32_ctz") == 0 then
        io_write_string "(i32.ctz)\n"

      elseif string_compare(name, "__i32_popcnt") == 0 then
        io_write_string "(i32.popcnt)\n"

      elseif string_compare(name, "__unreachable") == 0 then
        io_write_string "(unreachable)\n"

      elseif string_compare(name, "__memory_size") == 0 then
        io_write_string "(memory.size)\n"

      elseif string_compare(name, "__memory_grow") == 0 then
        io_write_string "(memory.grow)\n"

      elseif string_compare(name, "__memory_copy") == 0 then
        io_write_string "(memory.copy)\n"

      elseif string_compare(name, "__memory_fill") == 0 then
        io_write_string "(memory.fill)\n"

      elseif string_compare(name, "__export_start") == 0 then
        local arg = get_items(items[2])[1]
        local arg_ref = get_attr(arg, attr_ref)
        io_write_string '(export "_start" (func $'
        io_write_integer(get_attr(arg_ref, attr_id))
        io_write_string "))\n"
      end
    else
      io_write_string "(call $"
      io_write_integer(get_attr(ref, attr_id))
      io_write_string ") (; "
      io_write_string(get_value(ref))
      io_write_string ";)\n"
    end

    -- 関数呼び出し文の場合は返り値をすべて破棄する
    if not get_attr(v, attr_exp) then
      for i = 1, result do
        io_write_string "(drop)\n"
      end
    end

  elseif string_compare(kind, "if") == 0 then
    io_write_string "if\n"
    process3(ctx, proto, v, items[2])
    io_write_string "else\n"
    process3(ctx, proto, v, items[3])
    io_write_string "end\n"

  elseif string_compare(kind, "break") == 0 then
    io_write_string "(br $"
    io_write_integer(get_attr(v, attr_id))
    io_write_string ")\n"

  elseif string_compare(kind, "while") == 0 then
    local loop = get_attr(v, attr_ref)
    io_write_string "(br $"
    io_write_integer(loop[loop_loop])
    io_write_string ")\n"
    io_write_string "end\n"
    io_write_string "end\n"

  elseif string_compare(kind, "repeat") == 0 then
    local loop = get_attr(v, attr_ref)
    io_write_string "(i32.eqz)\n"
    io_write_string "(br_if $"
    io_write_integer(loop[loop_loop])
    io_write_string ")\n"
    io_write_string "end\n"
    io_write_string "end\n"

  elseif string_compare(kind, "for") == 0 then
    local loop = get_attr(v, attr_ref)
    io_write_string "(br $"
    io_write_integer(loop[loop_loop])
    io_write_string ")\n"
    io_write_string "end\n"
    io_write_string "end\n"

  elseif string_compare(kind, "function") == 0 then
    io_write_string "end\n"
    local result = get_attr(proto, attr_result)
    for i = 1, result do
      io_write_string "(local.get $r"
      io_write_integer(i)
      io_write_string ")\n"
    end
    io_write_string "(return)\n"

    io_write_string ")\n"

  elseif string_compare(kind, "local") == 0 then
    if proto ~= nil then
      local namelist = get_items(items[2])
      for i = #namelist, 1, -1 do
        local var = namelist[i]
        io_write_string "(local.set $"
        io_write_integer(get_attr(var, attr_id))
        io_write_string ") (; "
        io_write_string(get_value(var))
        io_write_string " ;)\n"
      end
    end

  elseif string_compare(kind, "return") == 0 then
    local result = get_attr(proto, attr_result)
    for i = result, 1, -1 do
      io_write_string "(local.set $r"
      io_write_integer(i)
      io_write_string ")\n"
    end
    io_write_string "(br $main)\n"

  elseif string_compare(kind, "table") == 0 then
    for i = #get_items(v), 1, -1 do
      io_write_string "(local.get $"
      io_write_integer(get_attr(v, attr_id))
      io_write_string ")\n"
      io_write_string "(i32.const "
      io_write_integer(i)
      io_write_string ")\n"
      io_write_string "(call $"
      io_write_integer(get_attr(ctx[ctx_set_table], attr_id))
      io_write_string ") (; __set_table ;)\n"
    end

  elseif string_compare(kind, "not") == 0 then
    io_write_string "(i32.eqz)\n"

  elseif string_compare(kind, "#") == 0 then
    io_write_string "(call $"
    io_write_integer(get_attr(ctx[ctx_length], attr_id))
    io_write_string ") (; __length ;)\n"

  elseif string_compare(kind, "or") == 0 then
    io_write_string "end\n"

  elseif string_compare(kind, "and") == 0 then
    io_write_string "else\n"
    io_write_string "(local.get $dup)\n"
    io_write_string "end\n"

  elseif string_compare(kind, "<") == 0 then
    io_write_string "(i32.lt_s)\n"

  elseif string_compare(kind, ">") == 0 then
    io_write_string "(i32.gt_s)\n"

  elseif string_compare(kind, "<=") == 0 then
    io_write_string "(i32.le_s)\n"

  elseif string_compare(kind, ">=") == 0 then
    io_write_string "(i32.ge_s)\n"

  elseif string_compare(kind, "~=") == 0 then
    io_write_string "(i32.ne)\n"

  elseif string_compare(kind, "==") == 0 then
    io_write_string "(i32.eq)\n"

  elseif string_compare(kind, "|") == 0 then
    io_write_string "(i32.or)\n"

  elseif string_compare(kind, "~") == 0 then
    -- bnot or bxor
    if #get_items(v) == 1 then
      io_write_string "(i32.const -1)\n"
    end
    io_write_string "(i32.xor)\n"

  elseif string_compare(kind, "&") == 0 then
    io_write_string "(i32.and)\n"

  elseif string_compare(kind, "<<") == 0 then
    io_write_string "(i32.shl)\n"

  elseif string_compare(kind, ">>") == 0 then
    -- Luaのshrはゼロ埋め
    io_write_string "(i32.shr_u)\n"

  elseif string_compare(kind, "..") == 0 then
    io_write_string "(call $"
    io_write_integer(get_attr(ctx[ctx_concat], attr_id))
    io_write_string ") (; __concat ;)\n"

  elseif string_compare(kind, "+") == 0 then
    io_write_string "(i32.add)\n"

  elseif string_compare(kind, "-") == 0 then
    io_write_string "(i32.sub)\n"

  elseif string_compare(kind, "*") == 0 then
    io_write_string "(i32.mul)\n"

  elseif string_compare(kind, "/") == 0 or string_compare(kind, "//") == 0 then
    io_write_string "(i32.div_s)\n"

  elseif string_compare(kind, "%") == 0 then
    io_write_string "(i32.rem_s)\n"

  elseif string_compare(kind, "^") == 0 then
    -- pow
    io_write_string "(call $"
    io_write_integer(get_attr(ctx[ctx_power], attr_id))
    io_write_string ") (; __power ;)\n"

  elseif string_compare(kind, "index") == 0 then
    if string_compare(get_attr(v, attr_resolver), "set") ~= 0 then
      io_write_string "(call $"
      io_write_integer(get_attr(ctx[ctx_get_table], attr_id))
      io_write_string ") (; __get_table ;)\n"
    end
  end
end

function write_proto_table(proto_table)
  local function_table = {}
  for i = 1, #proto_table do
    local proto = proto_table[i]
    if string_compare(get_attr(proto, attr_resolver), "fun") == 0 then
      table_insert(function_table, proto)
    end
  end
  if #function_table > 0 then
    io_write_string "(table "
    io_write_integer(#function_table + 1)
    io_write_string " funcref)\n"
    io_write_string "(elem (i32.const 1)"
    for i = 1, #function_table do
      local proto = function_table[i]
      if get_attr(proto, attr_address) ~= i then
        error "compiler error: invalid address"
      end
      io_write_string " $"
      io_write_integer(get_attr(proto, attr_id))
    end
    io_write_string ")\n"
  end
end

function compiler(tokens, chunk)
  local string_table, string_end = make_string_table(tokens)
  local heap_pointer = roundup(string_end, 1024)
  local memory_size = roundup(heap_pointer, 65536) >> 16

  local ctx = new_ctx()
  local proto_table = {}
  local var_table = {}
  local scope = new_scope(nil)

  add_asm(ctx, proto_table, new_name "__call_indirect0", 0)
  add_asm(ctx, proto_table, new_name "__call_indirect1", 1)
  add_asm(ctx, proto_table, new_name "__call_indirect2", 2)
  add_asm(ctx, proto_table, new_name "__call_indirect3", 3)
  add_asm(ctx, proto_table, new_name "__i32_load", 1)
  add_asm(ctx, proto_table, new_name "__i32_load8", 1)
  add_asm(ctx, proto_table, new_name "__i32_store", 0)
  add_asm(ctx, proto_table, new_name "__i32_store8", 0)
  add_asm(ctx, proto_table, new_name "__i32_clz", 1)
  add_asm(ctx, proto_table, new_name "__i32_ctz", 1)
  add_asm(ctx, proto_table, new_name "__i32_popcnt", 1)
  add_asm(ctx, proto_table, new_name "__unreachable", 0)
  add_asm(ctx, proto_table, new_name "__memory_size", 1)
  add_asm(ctx, proto_table, new_name "__memory_grow", 1)
  add_asm(ctx, proto_table, new_name "__memory_copy", 0)
  add_asm(ctx, proto_table, new_name "__memory_fill", 0)
  add_asm(ctx, proto_table, new_name "__export_start", 0)
  local fd_read_id = add_fun(ctx, proto_table, new_name "__fd_read", 1)
  local fd_write_id = add_fun(ctx, proto_table, new_name "__fd_write", 1)
  local heap_pointer_id = add_global(ctx, var_table, scope, new_name "__heap_pointer")

  local chunk_block = get_items(chunk)[1]
  process1(ctx, proto_table, nil, chunk, chunk_block)
  process2(ctx, proto_table, var_table, nil, scope, nil, chunk, chunk_block)

  ctx[ctx_length]    = resolve_name(proto_table, scope, new_name "__length")
  ctx[ctx_concat]    = resolve_name(proto_table, scope, new_name "__concat")
  ctx[ctx_power]     = resolve_name(proto_table, scope, new_name "__power")
  ctx[ctx_new_table] = resolve_name(proto_table, scope, new_name "__new_table")
  ctx[ctx_set_table] = resolve_name(proto_table, scope, new_name "__set_table")
  ctx[ctx_get_table] = resolve_name(proto_table, scope, new_name "__get_table")

  io_write_string "(module\n"

  io_write_string '(import "wasi_unstable" "fd_read" (func $'
  io_write_integer(fd_read_id)
  io_write_string " (param i32 i32 i32 i32) (result i32)))\n"

  io_write_string '(import "wasi_unstable" "fd_write" (func $'
  io_write_integer(fd_write_id)
  io_write_string " (param i32 i32 i32 i32) (result i32)))\n"

  io_write_string "(global $"
  io_write_integer(heap_pointer_id)
  io_write_string " (mut i32) (i32.const "
  io_write_integer(heap_pointer)
  io_write_string "))\n"

  io_write_string "(memory "
  io_write_integer(memory_size)
  io_write_string ")\n"

  io_write_string'(export "memory" (memory 0))\n'

  process3(ctx, nil, chunk, chunk_block)

  write_proto_table(proto_table)

  if #string_table > 0 then
    write_string_table(string_table)
  end

  io_write_string ")\n"
end

--------------------------------------------------------------------------------

function main()
  local source = io_read_all()
  local tokens = lexer(source)
  local chunk = parser(tokens)
  compiler(tokens, chunk)
end

__export_start(main)
