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

-- token {
--   kind     : string
--   attrs    : attrs
--   value    : string|integer
--   position : integer;
-- }
--
-- node {
--   kind  : string
--   attrs : attrs
--   ...   : (token|node)*
-- }

local attr_class    = 1 -- "token" | "node"
local attr_resolver = 2 -- "fun" | "var" | "par" | "ref" | "asm"
local attr_address  = 3 -- 文字列または関数の静的アドレス
local attr_id       = 4 -- 大域ID
local attr_result   = 5 -- 関数の返り値の個数
local attr_ref      = 6 -- 参照または変数テーブル

function new_token_attrs()
  return { "token", "", 0, 0, -1, nil }
end

function new_node_attrs()
  return { "node", "", 0, 0, -1, nil }
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
    return p, { v, new_token_attrs(), v, position }
  end
  return p, { "Name", new_token_attrs(), v, position }
end

function lexer_rule_symbol(source, position)
  for i = #lexer_symbols, 1, -1 do
    local symbols = lexer_symbols[i]
    local v = string_sub(source, position, position + i - 1)
    local j = binary_search(symbols, 1, #symbols, string_compare, v)
    if j ~= 0 then
      return position + i, { v, new_token_attrs(), v, position }
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
      return p, { "String", new_token_attrs(), string_char(t), position }
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
    return p, { "Integer", new_token_attrs(), v, position }
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

  table_insert(tokens, { "EOF", new_token_attrs(), "EOF", p })
  -- dump(tokens)
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

function nud_group(parser, token)
  local result = { "group", new_node_attrs(), parser_exp(parser, 0) }
  parser_expect(parser, ")")
  return result
end

function nud_table(parser, token)
  local result = { "table", new_node_attrs() }

  while true do
    local node = parser_exp_or_nil(parser, 0)
    if node == nil then
      break
    end
    table_insert(result, node)

    local token = parser_peek(parser)
    if string_compare(token[1], "}") == 0 then
      break
    end

    parser_expect2(parser, ",", ";")
  end
  parser_expect(parser, "}")

  return result
end

function nud_prefix(parser, token, lbp, node)
  return { token[1], new_node_attrs(), parser_exp(parser, parser_prefix_lbp) }
end

function led_left(parser, lbp, token, node)
  return { token[1], new_node_attrs(), node, parser_exp(parser, lbp) }
end

function led_right(parser, lbp, token, node)
  return { token[1], new_node_attrs(), node, parser_exp(parser, lbp - 1) }
end

function led_call(parser, lbp, token, node)
  local args = parser_list(parser, "args", parser_exp_or_nil, ",", ")")
  return { "call", new_node_attrs(), node, args }
end

function led_index(parser, lbp, token, node)
  local result = { "index", new_node_attrs(), node, parser_exp(parser, 0) }
  parser_expect(parser, "]")
  return result
end

function parser_initialize()
  parser_nud = {}
  parser_led = {}

  table_insert(parser_nud, { "false",   nud_token  })
  table_insert(parser_nud, { "nil",     nud_token  })
  table_insert(parser_nud, { "true",    nud_token  })
  table_insert(parser_nud, { "Name",    nud_token  })
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
  error("parser error at token <"..token[1].."> position "..integer_to_string(token[4]))
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
  if string_compare(token[1], kind) ~= 0 then
    parser_error(token)
  end
  parser[2] = index + 1
  return token
end

function parser_expect2(parser, kind1, kind2)
  local tokens = parser[1]
  local index = parser[2]
  local token = tokens[index]
  local kind = token[1]
  if not (string_compare(kind, kind1) == 0 or string_compare(kind, kind2) == 0) then
    parser_error(token)
  end
  parser[2] = index + 1
  return token
end

function parser_list(parser, kind, parse, separator, close)
  local result = { kind, new_node_attrs() }

  local i = 0
  while true do
    if i > 0 and separator ~= nil then
      if string_compare(parser_peek(parser)[1], separator) == 0 then
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
    table_insert(result, node)
  end

  if close ~= nil then
    parser_expect(parser, close)
  end

  return result
end

function parser_block(parser)
  return parser_list(parser, "block", parser_stat, nil, nil)
end

function parser_stat_assign(parser)
  local varlist = parser_list(parser, "varlist", parser_var, ",", "=")
  local explist = parser_list(parser, "explist", parser_exp_or_nil, ",", nil)
  return { "assign", new_node_attrs(), explist, varlist }
end

function parser_stat_if(parser)
  local exp = parser_exp(parser, 0)
  parser_expect(parser, "then")
  local then_block = parser_block(parser)
  local else_block = nil

  local token = parser_read(parser)
  if string_compare(token[1], "elseif") == 0 then
    else_block = { "block", new_node_attrs(), parser_stat_if(parser) }
  elseif string_compare(token[1], "else") == 0 then
    else_block = parser_block(parser)
  else
    parser_unread(parser)
    else_block = { "block", new_node_attrs() }
  end

  return { "if", new_node_attrs(), exp, then_block, else_block }
end

function parser_stat(parser)
  local token = parser_read(parser)
  if string_compare(token[1], ";") == 0 then
    return { ";", new_node_attrs() }

  elseif string_compare(token[1], "Name") == 0 then
    -- var ::= Name
    --       | Name       '[' exp ']' { '[' exp ']' }
    --       | Name args  '[' exp ']' { '[' exp ']' }
    --       | '(' exp ') '[' exp ']' { '[' exp ']' }
    if string_compare(parser_peek(parser)[1], "(") == 0 then
      local index = parser[2]
      parser_read(parser)
      local args = parser_list(parser, "args", parser_exp_or_nil, ",", ")")
      if string_compare(parser_peek(parser)[1], "[") ~= 0 then
        return { "call", new_node_attrs(), token, args }
      end
      parser[2] = index
    end
    parser_unread(parser)
    return parser_stat_assign(parser)

  elseif string_compare(token[1], "(") == 0 then
    parser_unread(parser)
    return parser_stat_assign(parser)

  elseif string_compare(token[1], "break") == 0 then
    return { "break", new_node_attrs() }

  elseif string_compare(token[1], "do") == 0 then
    local block = parser_block(parser)
    parser_expect(parser, "end")
    return { "do", new_node_attrs(), block }

  elseif string_compare(token[1], "while") == 0 then
    local exp = parser_exp(parser, 0)
    parser_expect(parser, "do")
    local block = parser_block(parser)
    parser_expect(parser, "end")
    return { "while", new_node_attrs(), exp, block }

  elseif string_compare(token[1], "repeat") == 0 then
    local block = parser_block(parser)
    parser_expect(parser, "until")
    local exp = parser_exp(parser, 0)
    return { "repeat", new_node_attrs(), block, exp }

  elseif string_compare(token[1], "if") == 0 then
    local result = parser_stat_if(parser)
    parser_expect(parser, "end")
    return result

  elseif string_compare(token[1], "for") == 0 then
    local name = parser_expect(parser, "Name")
    parser_expect(parser, "=")
    local exp1 = parser_exp(parser, 0)
    parser_expect(parser, ",")
    local exp2 = parser_exp(parser, 0)
    local exp3 = nil
    if string_compare(parser_peek(parser)[1], ",") == 0 then
      parser_read(parser)
      exp3 = parser_exp(parser, 0)
    else
      exp3 = { "Integer", new_token_attrs(), 1, 0 }
    end
    parser_expect(parser, "do")
    local block = parser_block(parser)
    parser_expect(parser, "end")
    return { "for", new_node_attrs(), exp1, exp2, exp3, name, block }

  elseif string_compare(token[1], "function") == 0 then
    local name = parser_expect(parser, "Name")
    parser_expect(parser, "(")
    local parlist = parser_list(parser, "parlist", parser_name, ",", ")")
    local block = parser_block(parser)
    parser_expect(parser, "end")
    return { "function", new_node_attrs(), name, parlist, block }

  elseif string_compare(token[1], "local") == 0 then
    -- 初期化無しのローカルは許可しない
    local namelist = parser_list(parser, "namelist", parser_name, ",", "=")
    local explist = parser_list(parser, "explist", parser_exp_or_nil, ",", nil)
    return { "local", new_node_attrs(), explist, namelist }

  elseif string_compare(token[1], "return") == 0 then
    local explist = parser_list(parser, "explist", parser_exp_or_nil, ",", nil)
    return { "return", new_node_attrs(), explist }

  else
    parser_unread(parser)
    return nil
  end
end

function parser_var(parser)
  local node = nil

  local token = parser_read(parser)
  if string_compare(token[1], "Name") == 0 then
    local next_token = parser_peek(parser)
    if string_compare(next_token[1], "(") == 0 then
      parser_read(parser)
      local args = parser_list(parser, "args", parser_exp_or_nil, ",", ")")
      node = { "call", new_node_attrs(), token, args }
    elseif string_compare(next_token[1], "[") ~= 0 then
      return token
    else
      node = token
    end
  elseif string_compare(token[1], "(") then
    node = nud_group(parser, token)
  else
    parser_unread(parser)
    return nil
  end

  while true do
    node = led_index(parser, parser_max_lbp, parser_expect(parser, "["), node)
    if string_compare(parser_peek(parser)[1], "[") ~= 0 then
      break
    end
  end

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
  if string_compare(parser_peek(parser)[1], "Name") == 0 then
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
  if string_compare(token[1], "EOF") ~= 0 then
    parser_error(token)
  end

  local chunk = { "chunk", new_node_attrs(), block }
  -- dump(chunk)
  return chunk
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
    if token[1] == "String" then
      table_insert(string_tokens, token)
    end
  end
  quick_sort(string_tokens, 1, #string_tokens, compare_string_index3)

  local string_table = {}
  local value = nil

  for i = 1, #string_tokens do
    local token = string_tokens[i]
    if value == nil or string_compare(value, token[3]) ~= 0 then
      value = token[3]
      table_insert(string_table, { value, 0 })
    end
    token[2][attr_address] = #string_table * 8
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
  error("out of range")
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
  io_write_string("(data 0 (i32.const 8) \"")

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

  io_write_string("\")\n")
end

function new_ctx()
  return { 0 }
end

local ctx_id = 1

function make_id(ctx)
  local id = ctx[ctx_id] + 1
  ctx[ctx_id] = id
  return id
end

function process1(ctx, proto_table, proto, u, v)
  if string_compare(v[1], "function") == 0 then
    if proto ~= nil then
      error("compiler error: invalid proto")
    end
    proto = v[3]
    add_fun(ctx, proto_table, proto, -1)

  elseif string_compare(v[1], "return") == 0 then
    local result = #v[3] - 2

    local proto_attrs = proto[2]
    if proto_attrs[attr_result] == -1 then
      proto_attrs[attr_result] = result
    elseif proto_attrs[attr_result] ~= result then
      error("compiler error: invalid result")
    end
  end

  if string_compare(v[2][attr_class], "node") == 0 then
    for i = 3, #v do
      process1(ctx, proto_table, proto, v, v[i])
    end
  end

  if string_compare(v[1], "function") == 0 then
    local proto_attrs = proto[2]
    if proto_attrs[attr_result] == -1 then
      proto[attr_result] = 0
    end
  end
end

function new_scope(parent)
  return { {}, parent }
end

local scope_data = 1
local scope_parent = 2

function new_name(name)
  return { "Name", new_token_attrs(), name, 0 }
end

function add_var_impl(ctx, var_table, scope, u, resolver)
  local id = make_id(ctx)
  local attrs = u[2]
  attrs[attr_resolver] = resolver
  attrs[attr_id] = id
  table_insert(var_table, u)
  table_insert(scope[scope_data], u)
  return id
end

function add_var(ctx, var_table, scope, u)
  return add_var_impl(ctx, var_table, scope, u, "var")
end

function add_par(ctx, var_table, scope, u)
  return add_var_impl(ctx, var_table, scope, u, "par")
end

function add_fun(ctx, proto_table, u, result)
  table_insert(proto_table, u)
  local id = make_id(ctx)
  local attrs = u[2]
  attrs[attr_resolver] = "fun"
  attrs[attr_address] = #proto_table
  attrs[attr_id] = id
  attrs[attr_result] = result
  attrs[attr_ref] = {}
  return id
end

function add_asm(ctx, proto_table, u, result)
  table_insert(proto_table, u)
  local attrs = u[2]
  attrs[attr_resolver] = resolver
  attrs[attr_result] = result
end

function resolve_name(proto_table, scope, u)
  while scope ~= nil do
    local data = scope[scope_data]
    for i = #data, 1, -1 do
      local v = data[i]
      if string_compare(u[3], v[3]) == 0 then
        u[2][attr_resolver] = "ref"
        u[2][attr_ref] = v
        return v
      end
    end
    scope = scope[scope_parent]
  end

  for i = 1, #proto_table do
    local v = proto_table[i]
    if string_compare(u[3], v[3]) == 0 then
      u[2][attr_resolver] = "ref"
      u[2][attr_ref] = v
      return v
    end
  end

  error("compiler error: cannot resolve <"..u[3]..">")
end

function process2(ctx, proto_table, var_table, scope, u, v)
  if string_compare(v[1], "block") == 0 then
    -- then blockとelse blockにスコープを割り当てる
    if string_compare(u[1], "if") == 0 then
      scope = new_scope(scope)
    end

  elseif string_compare(v[1], "do") == 0 then
    scope = new_scope(scope)

  elseif string_compare(v[1], "while") == 0 then
    scope = new_scope(scope)

  elseif string_compare(v[1], "repeat") == 0 then
    scope = new_scope(scope)

  elseif string_compare(v[1], "for") == 0 then
    scope = new_scope(scope)

    v[2][attr_id] = add_var(ctx, var_table, scope, new_name("(var)"))
    add_var(ctx, var_table, scope, new_name("(limit)"))
    add_var(ctx, var_table, scope, new_name("(step)"))

  elseif string_compare(v[1], "function") == 0 then
    var_table = v[3][2][attr_ref]
    scope = new_scope(scope)

  elseif string_compare(v[1], "Name") == 0 then
    if string_compare(u[1], "for") == 0 or string_compare(u[1], "namelist") == 0 then
      add_var(ctx, var_table, scope, v)
    elseif string_compare(u[1], "parlist") == 0 then
      add_par(ctx, var_table, scope, v)
    end

    if string_compare(v[2][attr_resolver], "") == 0 then
      resolve_name(proto_table, scope, v)
    end
  end

  if string_compare(v[2][attr_class], "node") == 0 then
    for i = 3, #v do
      process2(ctx, proto_table, var_table, scope, v, v[i])
    end
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

  add_asm(ctx, proto_table, new_name("__call_indirect0"), 0)
  add_asm(ctx, proto_table, new_name("__call_indirect1"), 1)
  add_asm(ctx, proto_table, new_name("__call_indirect2"), 2)
  add_asm(ctx, proto_table, new_name("__call_indirect3"), 3)
  add_asm(ctx, proto_table, new_name("__i32_load"), 1)
  add_asm(ctx, proto_table, new_name("__i32_load8"), 1)
  add_asm(ctx, proto_table, new_name("__i32_store"), 0)
  add_asm(ctx, proto_table, new_name("__i32_store8"), 0)
  add_asm(ctx, proto_table, new_name("__unreachable"), 0)
  add_asm(ctx, proto_table, new_name("__memory_size"), 1)
  add_asm(ctx, proto_table, new_name("__memory_grow"), 1)
  add_asm(ctx, proto_table, new_name("__export_start"), 0)
  local fd_read_id = add_fun(ctx, proto_table, new_name("__fd_read"), 1)
  local fd_write_id = add_fun(ctx, proto_table, new_name("__fd_write"), 1)
  local heap_pointer_id = add_var(ctx, var_table, scope, new_name("__heap_pointer"))

  process1(ctx, proto_table, nil, chunk, chunk[3])
  process2(ctx, proto_table, var_table, scope, chunk, chunk[3])

  io_write_string('(module\n')

  io_write_string('(import "wasi_unstable" "fd_read" (func $')
  io_write_integer(fd_read_id)
  io_write_string(' (param i32 i32 i32 i32) (result i32)))\n')

  io_write_string('(import "wasi_unstable" "fd_write" (func $')
  io_write_integer(fd_write_id)
  io_write_string(' (param i32 i32 i32 i32) (result i32)))')

  io_write_string('(memory ')
  io_write_integer(memory_size)
  io_write_string(')\n')

  io_write_string('(export "memory" (memory 0))\n')

  io_write_string(')\n')

  -- dump(proto_table)
  -- dump(var_table)
  -- dump(chunk)
  -- write_string_table(string_table)
end

--------------------------------------------------------------------------------

function main()
  local source = io_read_all()
  local tokens = lexer(source)
  local chunk = parser(tokens)
  compiler(tokens, chunk)
end

__export_start(main)
