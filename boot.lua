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

local json = require "dromozoa.commons.json"

function quick_sort(t, i, j, compare)
  local n = j - i + 1
  if n <= 1 then
    return
  end

  local pivot = t[i + (n >> 1)]
  local a = i
  local b = j

  while a <= b do
    while call_indirect1(compare, t[a], pivot) < 0 do
      a = a + 1
    end
    while call_indirect1(compare, t[b], pivot) > 0 do
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
    local r = call_indirect1(compare, t[m], v)
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
    "in";
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
    return p, { v, v, position }
  end
  return p, { "Name", v, position }
end

function lexer_rule_symbol(source, position)
  for i = #lexer_symbols, 1, -1 do
    local symbols = lexer_symbols[i]
    local v = string_sub(source, position, position + i - 1)
    local j = binary_search(symbols, 1, #symbols, string_compare, v)
    if j ~= 0 then
      return position + i, { v, v, position }
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
      return p, { "String", string_char(t), position }
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
    r, v = call_indirect2(char_to_integer, string_byte(source, p), v)
    if not r then
      break
    end
    p = p + 1
  end

  if p == q then
    return 0, nil
  else
    return p, { "Integer", v, position }
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
      q, token = call_indirect2(lexer_rules[i], source, p)
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

  table_insert(tokens, { "EOF", "EOF", p })

  return tokens
end

--------------------------------------------------------------------------------

local parser_nud = nil
local parser_led = nil
local parser_prefix_lbp = 0

function nud_token(parser, token)
  return token
end

function nud_group(parser, token)
  local result = parser_exp(parser, 0, true)
  parser_expect(parser, ")")
  return result
end

function nud_table(parser, token)
  local result = { "Table" }

  while true do
    local node = parser_exp(parser, 0, false)
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
  return { token[1], parser_exp(parser, parser_prefix_lbp, true) }
end

function led_left(parser, lbp, token, node)
  return { token[1], node, parser_exp(parser, lbp, true) }
end

function led_right(parser, lbp, token, node)
  return { token[1], node, parser_exp(parser, lbp - 1, true) }
end

function led_call(parser, lbp, token, node)
  return { "call", node, parser_items(parser, "args", parser_items_exp, ",", ")") }
end

function parser_item_compare(a, b)
  return string_compare(a[1], b[1])
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
  table_insert(parser_led, { "(",   bp, led_call   }) bp = bp + 10

  quick_sort(parser_nud, 1, #parser_nud, parser_item_compare)
  quick_sort(parser_led, 1, #parser_led, parser_item_compare)
end

function parser_error(token)
  error("parser error at token <"..token[1].."> position "..integer_to_string(token[3]))
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

function parser_items(parser, kind, parse, separator, close)
  local result = { kind }

  while true do
    local node = call_indirect1(parse, parser)
    if node == nil then
      break
    end
    table_insert(result, node)

    if separator ~= nil then
      local token = parser_peek(parser)
      if string_compare(token[1], separator) ~= 0 then
        break
      end
      parser_read(parser)
    end
  end

  if close ~= nil then
    parser_expect(parser, close)
  end

  return result
end

function parser_items_exp(parser)
  return parser_exp(parser, 0, false)
end

function parser_search(t, item)
  local i = binary_search(t, 1, #t, parser_item_compare, item)
  if i == 0 then
    return nil
  else
    return t[i]
  end
end

function parser_exp(parser, rbp, error_if_no_nud)
  local token = parser_read(parser)
  local nud = parser_search(parser_nud, token)
  if nud == nil then
    if error_if_no_nud then
      parser_error(token)
    end
    parser_unread(parser)
    return nil
  end

  local node = call_indirect1(nud[2], parser, token)
  while true do
    local token = parser_peek(parser)
    local led = parser_search(parser_led, token)
    if led == nil or led[2] <= rbp then
      break
    end

    parser_read(parser)
    node = call_indirect1(led[3], parser, led[2], token, node)
  end
  return node
end

function parser(tokens)
  parser_initialize()

  local parser = { tokens, 1 }
  local tree = parser_exp(parser, 0, true)
  print(json.encode(tree, { pretty = true, stable = true }))
end

--------------------------------------------------------------------------------

function compiler(tokens, chunk)
end

--------------------------------------------------------------------------------

function main()
  local source = io_read_all()
  local tokens = lexer(source)
  local chunk = parser(tokens)
  compiler(tokens, chunk)
end

export_start(main)
