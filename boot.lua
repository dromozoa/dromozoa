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

require "runtime"

function io_write_string_repeat(n, s)
  for i = 1, n do
    io_write_string(s)
  end
end

function S(s)
  io_write_string(s)
end

function SR(n, s)
  io_write_string_repeat(n, s)
end

function I(v)
  io_write_integer(v)
end

--------------------------------------------------------------------------------

function quick_sort_impl(t, i, j, compare)
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

  quick_sort_impl(t, i, b, compare)
  quick_sort_impl(t, a, j, compare)
end

function quick_sort(t, compare)
  quick_sort_impl(t, 1, #t, compare)
end

function binary_search(t, compare, v)
  local i = 1
  local n = #t
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

function new_node_impl(kind, value, items, source_file, source_position)
  return {
    kind = kind;

    resolver = "";
    address = 0;
    id = 0;
    index = 0;
    result = -1;
    is_exp = false;
    is_global = false;
    ref = nil;

    value = value;
    items = items;
    source_file = source_file;
    source_position = source_position;
  }
end

function new_token(kind, value, source_file, source_position)
  return new_node_impl(kind, value, nil, source_file, source_position)
end

function new_name(name)
  return new_token("Name", name, "(internal)", 0)
end

function new_node(kind, items)
  local source_file = "(internal)"
  local source_position = 0
  for i = 1, #items do
    local item = items[i]
    local p = item.source_position
    if source_position == 0 or source_position > p then
      source_file = item.source_file
      source_position = p
    end
  end
  return new_node_impl(kind, nil, items, source_file, source_position)
end

function new_empty_node(kind, token)
  return new_node_impl(kind, nil, {}, token.source_file, token.source_position)
end

function get_at_string(u)
  return "at node ["..u.kind.."] position ["..u.source_file..":"..integer_to_string(u.source_position).."]"
end

--------------------------------------------------------------------------------

function string_compare_first(a, b)
  return string_compare(a[1], b[1])
end

function string_compare_value(a, b)
  return string_compare(a.value, b.value)
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
      ".";
      ";";
      "<";
      "=";
      ">";
      "[";
      "]";
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

function lexer_error(file, position)
  error("lexer error at position ["..file..":"..integer_to_string(position).."]")
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

function lexer_rule_space(source_file, source, position)
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

function lexer_rule_comment(source_file, source, position)
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
      lexer_error(source_file, q)
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

function lexer_rule_keyword_or_name(source_file, source, position)
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
  local i = binary_search(lexer_keywords, string_compare, v)
  if i ~= 0 then
    return p, new_token(v, v, source_file, position)
  end
  return p, new_token("Name", v, source_file, position)
end

function lexer_rule_symbol(source_file, source, position)
  for i = #lexer_symbols, 1, -1 do
    local symbols = lexer_symbols[i]
    local v = string_sub(source, position, position + i - 1)
    local j = binary_search(symbols, string_compare, v)
    if j ~= 0 then
      return position + i, new_token(v, v, source_file, position)
    end
  end
  return 0, nil
end

function lexer_rule_string(source_file, source, position)
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
      return p, new_token("String", string_char(t), source_file, position)
    elseif c == 0x5C then
      if p > n then
        lexer_error(source_file, p)
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
          lexer_error(source_file, p)
        end
        local r = false
        local v = 0
        r, v = lexer_char_to_integer_hex(string_byte(source, p), v)
        if not r then
          lexer_error(source_file, p)
        end
        p = p + 1
        r, v = lexer_char_to_integer_hex(string_byte(source, p), v)
        if not r then
          lexer_error(source_file, p)
        end
        p = p + 1
        table_insert(t, v)
      else
        lexer_error(source_file, p - 1)
      end
    else
      table_insert(t, c)
    end
  end

  lexer_error(source_file, p)
end

function lexer_rule_integer(source_file, source, position)
  local p = position
  local n = #source
  local q = position
  local t = {}

  local read_char = function (c)
    return 0x30 <= c and c <= 0x39
  end

  local prefix = string_sub(source, p, p + 1)
  if string_compare(prefix, "0X") == 0 or string_compare(prefix, "0x") == 0 then
    p = p + 2
    q = q + 2

    -- WATの16進数リテラルは0xだけ
    table_insert(t, 0x30)
    table_insert(t, 0x78)

    read_char = function (c)
      return 0x30 <= c and c <= 0x39 or 0x41 <= c and c <= 0x46 or 0x61 <= c and c <= 0x66
    end
  end

  local r = false
  while p <= n do
    local c = string_byte(source, p)
    if not __call_indirect1(read_char, c) then
      break
    end
    p = p + 1
    table_insert(t, c)
  end

  if p == q then
    return 0, nil
  else
    return p, new_token("Integer", string_char(t), source_file, position)
  end
end

function lexer(source_file, source)
  local p = 1
  local n = #source

  local tokens = {}

  while p <= n do
    local q = 0
    local token = nil

    for i = 1, #lexer_rules do
      q, token = __call_indirect2(lexer_rules[i], source_file, source, p)
      if q ~= 0 then
        break
      end
    end

    if q == 0 then
      lexer_error(source_file, p)
    end
    p = q

    if token ~= nil then
      table_insert(tokens, token)
    end
  end

  table_insert(tokens, new_token("EOF", "EOF", source_file, p))
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
  if string_compare(parser_peek(parser).kind, "String") == 0 then
    local args = new_node("args", { parser_read(parser) })
    local result = new_node("call", { token, args })
    result.is_exp = true
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
  if string_compare(parser_peek(parser).kind, "Name") == 0 then
    local name = parser_read(parser)
    if string_compare(parser_peek(parser).kind, "=") == 0 then
      local items = {}
      while true do
        name.resolver = "key"
        parser_expect(parser, "=")
        table_insert(items, new_node("field", { name, parser_exp(parser, 0) }))
        local token = parser_peek(parser)
        if string_compare(token.kind, ",") == 0
          or string_compare(token.kind, ";") == 0 then
          parser_read(parser)
          token = parser_peek(parser)
        end
        if string_compare(token.kind, "}") == 0 then
          break
        end
        name = parser_expect(parser, "Name")
      end
      parser_expect(parser, "}")
      return new_node("table", items)
    end
    parser_unread(parser)
  end

  local items = {}
  while true do
    local node = parser_exp_or_nil(parser)
    if node == nil then
      break
    end
    table_insert(items, node)
    local token = parser_peek(parser)
    if string_compare(token.kind, "}") == 0 then
      break
    end
    parser_expect2(parser, ",", ";")
  end
  parser_expect(parser, "}")
  return new_node("array", items)
end

function nud_prefix(parser, token)
  return new_node(token.kind, { parser_exp(parser, parser_prefix_lbp) })
end

function nud_negate(parser, token)
  local exp = parser_exp(parser, parser_prefix_lbp)
  if string_compare(exp.kind, "Integer") == 0 then
    return new_token("Integer", "-"..exp.value, token.source_file, token.source_position)
  else
    return new_node(token.kind, { exp })
  end
end

function nud_function(parser, token)
  parser_expect(parser, "(")
  local parlist = parser_list(parser, "parlist", parser_name, ",", ")")
  local block = parser_block(parser)
  parser_expect(parser, "end")
  local result = new_node("function", { new_name "(unnamed)", parlist, block })
  result.is_exp = true
  return result
end

function led_left(parser, lbp, token, node)
  return new_node(token.kind, { node, parser_exp(parser, lbp) })
end

function led_right(parser, lbp, token, node)
  return new_node(token.kind, { node, parser_exp(parser, lbp - 1) })
end

function led_call(parser, lbp, token, node)
  local args = parser_list(parser, "args", parser_exp_or_nil, ",", ")")
  local result = new_node("call", { node, args })
  result.is_exp = true
  return result
end

function led_index(parser, lbp, token, node)
  local items = { node, parser_exp(parser, 0) }
  parser_expect(parser, "]")
  return new_node("index", items)
end

function led_key(parser, lbp, token, node)
  local result = led_left(parser, lbp, token, node)
  local name = result.items[2]
  if string_compare(name.kind, "Name") == 0 then
    name.resolver = "key"
  end
  return result
end

function parser_initialize()
  parser_nud = {}
  parser_led = {}

  table_insert(parser_nud, { "false",    nud_token    })
  table_insert(parser_nud, { "nil",      nud_token    })
  table_insert(parser_nud, { "true",     nud_token    })
  table_insert(parser_nud, { "Name",     nud_name     })
  table_insert(parser_nud, { "String",   nud_token    })
  table_insert(parser_nud, { "Integer",  nud_token    })
  table_insert(parser_nud, { "(",        nud_group    })
  table_insert(parser_nud, { "{",        nud_table    })
  table_insert(parser_nud, { "not",      nud_prefix   })
  table_insert(parser_nud, { "#",        nud_prefix   })
  table_insert(parser_nud, { "-",        nud_negate   })
  table_insert(parser_nud, { "~",        nud_prefix   })
  table_insert(parser_nud, { "function", nud_function })

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
  table_insert(parser_led, { "//",  bp, led_left   })
  table_insert(parser_led, { "%",   bp, led_left   }) bp = bp + 10
  parser_prefix_lbp = bp                              bp = bp + 10
  table_insert(parser_led, { "(",   bp, led_call   })
  table_insert(parser_led, { "[",   bp, led_index  })
  table_insert(parser_led, { ".",   bp, led_key    })
  parser_max_lbp = bp

  quick_sort(parser_nud, string_compare_first)
  quick_sort(parser_led, string_compare_first)
end

function parser_error(node)
  error("parser error "..get_at_string(node))
end

function parser_item_search(t, item)
  local i = binary_search(t, string_compare_first, { item.kind })
  if i == 0 then
    return nil
  else
    return t[i]
  end
end

function parser_peek(parser)
  return parser.tokens[parser.index]
end

function parser_read(parser)
  local token = parser.tokens[parser.index]
  parser.index = parser.index + 1
  return token
end

function parser_unread(parser)
  parser.index = parser.index - 1
end

function parser_expect(parser, kind)
  local token = parser.tokens[parser.index]
  if string_compare(token.kind, kind) ~= 0 then
    parser_error(token)
  end
  parser.index = parser.index + 1
  return token
end

function parser_expect2(parser, kind1, kind2)
  local token = parser.tokens[parser.index]
  local kind = token.kind
  if not (string_compare(kind, kind1) == 0 or string_compare(kind, kind2) == 0) then
    parser_error(token)
  end
  parser.index = parser.index + 1
  return token
end

function parser_list(parser, kind, parse, separator, close)
  local items = {}

  local i = 0
  while true do
    if i > 0 and separator ~= nil then
      if string_compare(parser_peek(parser).kind, separator) == 0 then
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
  if string_compare(token.kind, "elseif") == 0 then
    else_block = new_node("block", { parser_stat_if(parser) })
  elseif string_compare(token.kind, "else") == 0 then
    else_block = parser_block(parser)
  else
    parser_unread(parser)
    else_block = new_empty_node("block", token)
  end

  return new_node("if", { exp, then_block, else_block })
end

function parser_stat(parser)
  local token = parser_read(parser)
  if string_compare(token.kind, ";") == 0 then
    return new_empty_node(";", token)

  elseif string_compare(token.kind, "Name") == 0 then
    -- var ::= Name
    --       | Name       '[' exp ']' { '[' exp ']' }
    --       | Name args  '[' exp ']' { '[' exp ']' }
    --       | '(' exp ') '[' exp ']' { '[' exp ']' }
    -- args ::= '(' ... ')'
    --        | String
    local next_token = parser_peek(parser)
    if string_compare(next_token.kind, "(") == 0
      or string_compare(next_token.kind, "String") == 0 then
      local index = parser.index
      parser_read(parser)
      local args = nil
      if string_compare(next_token.kind, "(") == 0 then
        args = parser_list(parser, "args", parser_exp_or_nil, ",", ")")
      else
        args = new_node("args", { next_token })
      end
      if string_compare(parser_peek(parser).kind, "[") ~= 0
        and string_compare(parser_peek(parser).kind, ".") ~= 0 then
        -- 関数呼び出し文のrequireだけ特別扱いする
        if string_compare(token.value, "require") == 0 then
          local arg = args.items[1]
          if string_compare(arg.kind, "String") == 0 then
            local loaded = parser.loaded
            local name = arg.value
            local found = false
            for i = 1, #loaded do
              if string_compare(loaded[i], name) == 0 then
                found = true
              end
            end
            if found then
              return new_empty_node(";", token)
            else
              local chunk = lexer_parser("include-wasm/"..name..".lua", loaded)
              table_insert(loaded, name)
              return chunk.items[1]
            end
          end
        end
        return new_node("call", { token, args })
      end
      parser.index = index
    end
    parser_unread(parser)
    return parser_stat_assign(parser)

  elseif string_compare(token.kind, "(") == 0 then
    parser_unread(parser)
    return parser_stat_assign(parser)

  elseif string_compare(token.kind, "break") == 0 then
    return new_empty_node("break", token)

  elseif string_compare(token.kind, "do") == 0 then
    local block = parser_block(parser)
    parser_expect(parser, "end")
    return new_node("do", { block })

  elseif string_compare(token.kind, "while") == 0 then
    local exp = parser_exp(parser, 0)
    parser_expect(parser, "do")
    local block = parser_block(parser)
    parser_expect(parser, "end")
    return new_node("while", { exp, block })

  elseif string_compare(token.kind, "repeat") == 0 then
    local block = parser_block(parser)
    parser_expect(parser, "until")
    local exp = parser_exp(parser, 0)
    return new_node("repeat", { block, exp })

  elseif string_compare(token.kind, "if") == 0 then
    local result = parser_stat_if(parser)
    parser_expect(parser, "end")
    return result

  elseif string_compare(token.kind, "for") == 0 then
    local name = parser_expect(parser, "Name")
    parser_expect(parser, "=")
    local exp1 = parser_exp(parser, 0)
    parser_expect(parser, ",")
    local exp2 = parser_exp(parser, 0)
    local exp3 = nil
    if string_compare(parser_peek(parser).kind, ",") == 0 then
      parser_read(parser)
      exp3 = parser_exp(parser, 0)
    else
      exp3 = new_token("Integer", "1", exp2.source_file, exp2.source_position)
    end
    parser_expect(parser, "do")
    local block = parser_block(parser)
    parser_expect(parser, "end")
    return new_node("for", { exp1, exp2, exp3, name, block })

  elseif string_compare(token.kind, "function") == 0 then
    local name = parser_expect(parser, "Name")
    parser_expect(parser, "(")
    local parlist = parser_list(parser, "parlist", parser_name, ",", ")")
    local block = parser_block(parser)
    parser_expect(parser, "end")
    return new_node("function", { name, parlist, block })

  elseif string_compare(token.kind, "local") == 0 then
    -- 初期化無しのローカルは許可しない
    local namelist = parser_list(parser, "namelist", parser_name, ",", "=")
    local explist = parser_list(parser, "explist", parser_exp_or_nil, ",", nil)
    return new_node("local", { explist, namelist })

  elseif string_compare(token.kind, "return") == 0 then
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
  if string_compare(token.kind, "Name") == 0 then
    local next_token = parser_peek(parser)
    if string_compare(next_token.kind, "(") == 0 then
      parser_read(parser)
      local args = parser_list(parser, "args", parser_exp_or_nil, ",", ")")
      node = new_node("call", { token, args })
      node.is_exp = true
    elseif string_compare(next_token.kind, "String") == 0 then
      parser_read(parser)
      local args = new_node("args", { next_token })
      node = new_node("call", { token, args })
      node.is_exp = true
    elseif string_compare(next_token.kind, "[") ~= 0
      and string_compare(next_token.kind, ".") ~= 0 then
      return token
    else
      node = token
    end
  elseif string_compare(token.kind, "(") then
    node = nud_group(parser, token)
  else
    parser_unread(parser)
    return nil
  end

  repeat
    local token = parser_expect2(parser, "[", ".")
    if string_compare(token.kind, "[") == 0 then
      node = led_index(parser, parser_max_lbp, token, node)
    else
      node = led_key(parser, parser_max_lbp, token, node)
    end
  until string_compare(parser_peek(parser).kind, "[") ~= 0
    and string_compare(parser_peek(parser).kind, ".") ~= 0

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
  if string_compare(parser_peek(parser).kind, "Name") == 0 then
    return parser_read(parser)
  else
    return nil
  end
end

function parser(tokens, loaded)
  local parser = {
    tokens = tokens;
    index  = 1;
    loaded = loaded;
  }

  local block = parser_block(parser)

  local token = parser_peek(parser)
  if string_compare(token.kind, "EOF") ~= 0 then
    parser_error(token)
  end

  return new_node("chunk", { block })
end

--------------------------------------------------------------------------------

local asm_table = nil
local op_table = nil

function compiler_initialize()
  local leave_call_indirect = function (ctx, proto, u)
    local items = u.items
    local args = items[2].items
    for i = 2, #args do
      process3(ctx, proto, items[2], args[i])
    end
    process3(ctx, proto, items[2], args[1])

    S"(call_indirect"
    if #args > 1 then
      S" (param" SR(#args - 1, " i32") S")"
    end

    local result = items[1].ref.result
    if result > 0 then
      S" (result" SR(result, " i32") S")"
    end

    S")\n"
  end

  local leave_export_start = function (ctx, proto, u)
    local items = u.items
    local args = items[2].items
    S'(export "_start" (func $' I(args[1].ref.id) S"))\n"
  end

  local leave_i64_const = function (ctx, proto, u)
    local items = u.items
    local args = items[2].items
    S"(i64.const " S(args[1].value) S") (; Integer ;)\n"
  end

  asm_table = {
    { "__call_indirect0", 0, nil,                  true,  leave_call_indirect };
    { "__call_indirect1", 1, nil,                  true,  leave_call_indirect };
    { "__call_indirect2", 2, nil,                  true,  leave_call_indirect };
    { "__call_indirect3", 3, nil,                  true,  leave_call_indirect };
    { "__i32_load",       1, "(i32.load)",         false, nil                 };
    { "__i32_load8",      1, "(i32.load8_u)",      false, nil                 };
    { "__i32_store",      0, "(i32.store)",        false, nil                 };
    { "__i32_store8",     0, "(i32.store8)",       false, nil                 };
    { "__i32_clz",        1, "(i32.clz)",          false, nil                 };
    { "__i32_ctz",        1, "(i32.ctz)",          false, nil                 };
    { "__i32_popcnt",     1, "(i32.popcnt)",       false, nil                 };
    { "__unreachable",    0, "(unreachable)",      false, nil                 };
    { "__memory_size",    1, "(memory.size)",      false, nil                 };
    { "__memory_grow",    1, "(memory.grow)",      false, nil                 };
    { "__memory_copy",    0, "(memory.copy)",      false, nil                 };
    { "__memory_fill",    0, "(memory.fill)",      false, nil                 };
    { "__export_start",   0, nil,                  true,  leave_export_start  };
    { "__i64_const",      1, nil,                  true,  leave_i64_const     };
    { "__i64_extend_i32", 1, "(i64.extend_i32_s)", false, nil                 };
  }
  quick_sort(asm_table, string_compare_first)

  op_table = {
    { "nil",   "(i32.const 0) (; nil ;)"   };
    { "false", "(i32.const 0) (; false ;)" };
    { "true",  "(i32.const 1) (; true ;)"  };
    { "not",   "(i32.eqz)"                 };
    { "<",     "(i32.lt_s)"                };
    { ">",     "(i32.gt_s)"                };
    { "<=",    "(i32.le_s)"                };
    { ">=",    "(i32.ge_s)"                };
    { "~=",    "(i32.ne)"                  };
    { "==",    "(i32.eq)"                  };
    { "|",     "(i32.or)"                  };
    { "&",     "(i32.and)"                 };
    { "<<",    "(i32.shl)"                 };
    { ">>",    "(i32.shr_u)"               };
    { "+",     "(i32.add)"                 };
    { "-",     "(i32.sub)"                 };
    { "*",     "(i32.mul)"                 };
    { "//",    "(i32.div_s)"               };
    { "%",     "(i32.rem_s)"               };
  }
  quick_sort(op_table, string_compare_first)
end

function compiler_error(message, u)
  error("compiler error: "..message.." "..get_at_string(u))
end

function roundup(n, a)
  local r = n % a
  if r == 0 then
    return n
  else
    return n + a - r
  end
end

function make_string_table(string_tokens)
  quick_sort(string_tokens, string_compare_value)

  local string_table = {}
  local value = nil

  for i = 1, #string_tokens do
    local token = string_tokens[i]
    if value == nil or string_compare(value, token.value) ~= 0 then
      value = token.value
      table_insert(string_table, { value, 0 })
    end
    token.address = #string_table * 8
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
  S'(data 0 (i32.const 8) "'

  for i = 1, #string_table do
    local entry = string_table[i]
    local t = {}
    encode_integer(t, #entry[1])
    encode_integer(t, entry[2])
    S(string_char(t))
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
    S(string_char(t))
  end

  S'")\n'
end

function new_ctx()
  return {
    id = 0;
    address = 0;
    length = nil;
    concat = nil;
    new_table = nil;
    set_index = nil;
    get_index = nil;
    set_table = nil;
    get_table = nil;
  }
end

function make_id(ctx)
  local id = ctx.id + 1
  ctx.id = id
  return id
end

function make_address(ctx)
  local address = ctx.address + 1
  ctx.address = address
  return address
end

function new_scope(parent)
  return { {}, parent }
end

local scope_data = 1
local scope_parent = 2

function add_var_impl(ctx, var_table, scope, u, resolver, is_global)
  local id = make_id(ctx)
  u.resolver = resolver
  u.id = id
  u.is_global = is_global
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
  u.resolver = "fun"
  u.address = make_address(ctx)
  u.id = id
  u.result = result
  u.ref = {}
  return id
end

function add_asm(ctx, proto_table, u, result)
  table_insert(proto_table, u)
  u.resolver = "asm"
  u.result = result
end

function add_wasi(ctx, proto_table, u, result, name, param)
  local id = add_fun(ctx, proto_table, u, result)
  S'(import "wasi_unstable" "' S(name) S'" (func $' I(id) S" " S(param) S" "
  if result > 0 then
    S"(result" SR(result, " i32") S")"
  end
  S"))\n"
end

function resolve_name_impl(proto_table, scope, u, resolver)
  while scope ~= nil do
    local data = scope[scope_data]
    for i = #data, 1, -1 do
      local v = data[i]
      if string_compare(u.value, v.value) == 0 then
        u.resolver = resolver
        u.ref = v
        return v
      end
    end
    scope = scope[scope_parent]
  end

  for i = 1, #proto_table do
    local v = proto_table[i]
    if string_compare(u.value, v.value) == 0 then
      u.resolver = resolver
      u.ref = v
      return v
    end
  end

  compiler_error("cannot resolve <"..u.value..">", u)
end

function resolve_name(proto_table, scope, u)
  return resolve_name_impl(proto_table, scope, u, "ref")
end

function resolve_call(proto_table, scope, u)
  return resolve_name_impl(proto_table, scope, u, "call")
end

function new_loop(ctx, u)
  local loop = {
    block = make_id(ctx);
    loop = make_id(ctx);
  }
  u.ref = loop
  return loop
end

function new_result_table(proto_table)
  local result_table = {}
  for i = 1, #proto_table do
    local proto = proto_table[i]
    if string_compare(proto.resolver, "fun") == 0 then
      table_insert(result_table, { proto.result })
      if proto.address ~= #result_table then
        error "compiler error: invalid address"
      end
    end
  end
  return result_table
end

function solve_result_table(result_table, function_table)
  for i = 1, #result_table do
    local r = result_table[i]
    if #r == 1 and r[1] == -1 then
      r[1] = 0
    end
  end

  while true do
    local m = 0
    local n = 0
    for i = 1, #result_table do
      local r = result_table[i]
      if #r == 1 then
        m = m + 1
      else
        local result = r[1]
        for j = 2, #r do
          local q = result_table[r[j]]
          if #q == 1 then
            result = result + q[1]
          else
            result = -1
            break
          end
        end
        if result ~= -1 then
          n = n + 1
          result_table[i] = { result }
        end
      end
    end
    if n == 0 then
      if m == #result_table then
        break
      end
      error "compiler error: invalid result"
    end
  end

  for i = 1, #function_table do
    local proto = function_table[i].items[1]
    local r = result_table[proto.address]
    if proto.result == -1 then
      proto.result = r[1]
    end
  end
end

function process1(ctx, string_tokens, proto_table, function_table, proto, u, v)
  local kind = v.kind
  local items = v.items

  if string_compare(kind, "function") == 0 then
    proto = items[1]
    add_fun(ctx, proto_table, proto, -1)
    table_insert(function_table, v)
  elseif string_compare(kind, "Name") == 0 then
    if string_compare(v.resolver, "key") == 0 then
      table_insert(string_tokens, v)
    end
  elseif string_compare(kind, "String") == 0 then
    table_insert(string_tokens, v)
  end

  if items ~= nil then
    for i = 1, #items do
      process1(ctx, string_tokens, proto_table, function_table, proto, v, items[i])
    end
  end
end

function process2(ctx, proto_table, var_table, result_table, proto, chunk_scope, scope, loop, u, v)
  local kind = v.kind
  local items = v.items

  if string_compare(kind, "block") == 0 then
    -- then blockとelse blockにスコープを割り当てる
    if string_compare(u.kind, "if") == 0 then
      scope = new_scope(scope)
    end

  elseif string_compare(kind, "assign") == 0 then
    local varlist = items[2].items
    for i = 1, #varlist do
      local var = varlist[i]
      if string_compare(var.kind, "Name") == 0 then
        resolve_name(proto_table, scope, var)
      end
      var.resolver = "set"
    end

  elseif string_compare(kind, "break") == 0 then
    if loop == nil then
      compiler_error("invalid loop", v)
    end
    v.id = loop.block

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

    v.id = add_var(ctx, var_table, scope, new_name "(var)")
    add_var(ctx, var_table, scope, new_name "(limit)")
    add_var(ctx, var_table, scope, new_name "(step)")

    add_var(ctx, var_table, scope, items[4])

  elseif string_compare(kind, "function") == 0 then
    proto = items[1]
    var_table = proto.ref
    scope = new_scope(chunk_scope)

  elseif string_compare(kind, "Name") == 0 then
    if string_compare(u.kind, "namelist") == 0 then
      if proto == nil then
        add_global(ctx, var_table, scope, v)
      else
        add_var(ctx, var_table, scope, v)
      end
    elseif string_compare(u.kind, "parlist") == 0 then
      add_par(ctx, var_table, scope, v)
    end

    if string_compare(v.resolver, "") == 0 then
      if string_compare(u.kind, "call") == 0 then
        resolve_call(proto_table, scope, v)
      else
        resolve_name(proto_table, scope, v)
      end
    end

  elseif string_compare(kind, "explist") == 0 or string_compare(kind, "args") == 0 then
    v.result = #items

  elseif string_compare(kind, "array") == 0 then
    v.id = add_var(ctx, var_table, scope, new_name "(array)")
    v.result = #items

  elseif string_compare(kind, "table") == 0 then
    v.id = add_var(ctx, var_table, scope, new_name "(table)")
    v.result = #items

  elseif string_compare(kind, "field") == 0 then
    v.id = add_var(ctx, var_table, scope, new_name "(field)")
    v.result = #items
  end

  if items ~= nil then
    for i = 1, #items do
      process2(ctx, proto_table, var_table, result_table, proto, chunk_scope, scope, loop, v, items[i])
    end
  end

  if string_compare(kind, "explist") == 0 then
    if string_compare(u.kind, "return") == 0 then
      local q = { 0 }
      for i = 1, #items do
        local item = items[i]
        if string_compare(item.kind, "call") == 0 then
          local ref = item.items[1].ref
          local result = ref.result
          if result == -1 then
            table_insert(q, ref.address)
          else
            q[1] = q[1] + result
          end
        else
          q[1] = q[1] + 1
        end
      end

      local address = proto.address
      local r = result_table[address]
      if #r == 1 then
        if r[1] == -1 then
          result_table[address] = q
        elseif #q == 1 and r[1] ~= q[1] then
          compiler_error("invalid result <"..proto.value..">", v)
        end
      else
        result_table[address] = q
      end
    end
  end
end

function process3(ctx, proto, u, v)
  local kind = v.kind
  local items = v.items

  local range_i = 1
  local range_j = 0
  if items ~= nil then
    range_j = #items
  end

  if string_compare(kind, "assign") == 0 then
    range_j = 1

  elseif string_compare(kind, "call") == 0 then
    if string_compare(items[1].kind, "Name") ~= 0 then
      compiler_error("invalid callee", items[1])
    end
    range_i = 2

    local ref = items[1].ref
    if string_compare(ref.resolver, "asm") == 0 then
      local name = ref.value
      local i = binary_search(asm_table, string_compare_first, { name })
      if i == 0 then
        compiler_error("cannot resolve <"..name.value..">", name)
      end
      if asm_table[i][4] then
        range_j = 0
      end
    end

  elseif string_compare(kind, "if") == 0 then
    range_j = 1

  elseif string_compare(kind, "while") == 0 then
    range_i = 2

    local loop = v.ref
    S"block $" I(loop.block) S"\n"
    S"loop $" I(loop.loop) S"\n"

    process3(ctx, proto, v, items[1])

    S"(i32.eqz)\n"
    S"(br_if $" I(loop.block) S")\n"

  elseif string_compare(kind, "repeat") == 0 then
    local loop = v.ref
    S"block $" I(loop.block) S"\n"
    S"loop $" I(loop.loop) S"\n"

  elseif string_compare(kind, "for") == 0 then
    range_i = 5

    local loop = v.ref
    S"block $" I(loop.block) S"\n"

    process3(ctx, proto, v, items[1])
    process3(ctx, proto, v, items[2])
    process3(ctx, proto, v, items[3])

    local var = v.id
    S"(local.set $" I(var + 2) S")\n"
    S"(local.set $" I(var + 1) S")\n"
    S"(local.set $" I(var + 0) S")\n"

    S"(local.get $" I(var + 0) S")\n"
    S"(local.get $" I(var + 2) S")\n"
    S"(i32.sub)\n"
    S"(local.set $" I(var + 0) S")\n"

    S"loop $" I(loop.loop) S"\n"

    S"(local.get $" I(var + 0) S")\n"
    S"(local.get $" I(var + 2) S")\n"
    S"(i32.add)\n"
    S"(local.set $" I(var + 0) S")\n"

    S"(local.get $" I(var + 2) S")\n"
    S"(i32.const 0)\n"
    S"(i32.ge_s)\n"

    S"if\n"
      S"(local.get $" I(var + 0) S")\n"
      S"(local.get $" I(var + 1) S")\n"
      S"(i32.gt_s)\n"
      S"(br_if $" I(loop.block) S")\n"
    S"else\n"
      S"(local.get $" I(var + 0) S")\n"
      S"(local.get $" I(var + 1) S")\n"
      S"(i32.lt_s)\n"
      S"(br_if $" I(loop.block) S")\n"
    S"end\n"

    S"(local.get $" I(var + 0) S")\n"
    S"(local.set $" I(items[4].id) S")\n"

  elseif string_compare(kind, "function") == 0 then
    range_j = 0
    if v.is_exp then
      S"(i32.const " I(v.items[1].address) S")\n"
    end

  elseif string_compare(kind, "local") == 0 then
    if proto == nil then
      range_j = 0

      local explist = items[1].items
      local namelist = items[2].items
      if #namelist ~= #explist then
        compiler_error("invalid result", v)
      end

      for i = 1, #explist do
        local exp = explist[i]
        local name = namelist[i]

        S"(global $" I(name.id) S" (mut i32)\n"
        process3(ctx, proto, explist, exp)
        S") (; " S(name.value) S" ;)\n"
      end
    else
      range_j = 1
    end

  elseif string_compare(kind, "Name") == 0 then
    if string_compare(v.resolver, "ref") == 0 then
      local ref = v.ref
      if string_compare(ref.resolver, "fun") == 0 then
        S"(i32.const " I(ref.address) S") (; "
      else
        if ref.is_global then
          S"(global.get $"
        else
          S"(local.get $"
        end
        I(ref.id) S") (; "
      end
      S(v.value) S" ;)\n"
    end

  elseif string_compare(kind, "Integer") == 0 then
    S"(i32.const " S(v.value) S") (; Integer ;)\n"

  elseif string_compare(kind, "String") == 0 then
    S"(i32.const " I(v.address) S") (; String ;)\n"

  elseif string_compare(kind, "array") == 0 then
    S"(i32.const " I(v.result) S")\n"
    S"(call $" I(ctx.new_table.id) S") (; __new_table ;)\n"
    S"(local.tee $" I(v.id) S")\n"

  elseif string_compare(kind, "table") == 0 then
    S"(i32.const " I(#items) S")\n"
    S"(call $" I(ctx.new_table.id) S") (; __new_table ;)\n"
    S"(local.tee $" I(v.id) S")\n"

  elseif string_compare(kind, "field") == 0 then
    range_i = 2
    S"(i32.const 2)\n"
    S"(call $" I(ctx.new_table.id) S") (; __new_table ;)\n"
    S"(local.tee $" I(v.id) S")\n"
    S"(i32.const " I(items[1].address) S") (; key ;)\n"

  elseif string_compare(kind, "or") == 0 then
    range_i = 2

    process3(ctx, proto, v, items[1])
    S"(local.tee $dup)\n"
    S"if (result i32)\n"
    S"(local.get $dup)\n"
    S"else\n"

  elseif string_compare(kind, "and") == 0 then
    range_i = 2

    process3(ctx, proto, v, items[1])
    S"(local.tee $dup)\n"
    S"if (result i32)\n"

  elseif string_compare(kind, "-") == 0 then
    if #v.items == 1 then
      S"(i32.const 0)\n"
    end

  elseif string_compare(kind, ".") == 0 then
    range_j = 1
  end

  if items ~= nil then
    for i = range_i, range_j do
      process3(ctx, proto, v, items[i])
    end
  end

  local i = binary_search(op_table, string_compare_first, { kind })
  if i ~= 0 then
    S(op_table[i][2])
    S"\n"

  elseif string_compare(kind, "assign") == 0 then
    local explist = items[1].items
    local varlist = items[2].items

    local result = items[1].result
    if result < #varlist then
      compiler_error("invalid result", v)
    end
    SR(result - #varlist, "(drop)\n")

    for i = #varlist, 1, -1 do
      local var = varlist[i]
      if string_compare(var.kind, "Name") == 0 then
        local ref = var.ref
        if ref.is_global then
          S"(global.set $"
        else
          S"(local.set $"
        end
        I(ref.id) S") (; " S(var.value) S" ;)\n"
      elseif string_compare(var.kind, "index") == 0 then
        process3(ctx, proto, items[2], var)
        S"(call $" I(ctx.set_index.id) S") (; __set_index ;)\n"
      elseif string_compare(var.kind, ".") == 0 then
        process3(ctx, proto, items[2], var)
        S"(i32.const " I(var.items[2].address) S") (; key ;)\n"
        S"(call $" I(ctx.set_table.id) S") (; __set_table ;)\n"

      else
        compiler_error("invalid assign <"..var.kind..">", var)
      end
    end

  elseif string_compare(kind, "call") == 0 then
    local ref = items[1].ref
    local name = ref.value
    if string_compare(ref.resolver, "asm") == 0 then
      local i = binary_search(asm_table, string_compare_first, { name })
      local asm = asm_table[i]
      if asm[5] ~= nil then
        __call_indirect0(asm[5], ctx, proto, v)
      else
        S(asm[3]) S"\n"
      end
    else
      S"(call $" I(ref.id) S") (; " S(name) S" ;)\n"
    end

    local result = ref.result
    if v.is_exp then
      if string_compare(u.kind, "explist") == 0
        or string_compare(u.kind, "args") == 0
        or string_compare(u.kind, "array") == 0 then
        u.result = u.result + result - 1
      elseif result == 0 then
        compiler_error("invalid result", v)
      else
        SR(result - 1, "(drop)\n")
      end
    else
      -- 関数呼び出し文の場合は返り値を破棄する
      SR(result, "(drop)\n")
    end

  elseif string_compare(kind, "if") == 0 then
    S"if\n"
    process3(ctx, proto, v, items[2])
    S"else\n"
    process3(ctx, proto, v, items[3])
    S"end\n"

  elseif string_compare(kind, "break") == 0 then
    S"(br $" I(v.id) S")\n"

  elseif string_compare(kind, "while") == 0 then
    local loop = v.ref
    S"(br $" I(loop.loop) S")\n"
    S"end\n"
    S"end\n"

  elseif string_compare(kind, "repeat") == 0 then
    local loop = v.ref
    S"(i32.eqz)\n"
    S"(br_if $" I(loop.loop) S")\n"
    S"end\n"
    S"end\n"

  elseif string_compare(kind, "for") == 0 then
    local loop = v.ref
    S"(br $" I(loop.loop) S")\n"
    S"end\n"
    S"end\n"

  elseif string_compare(kind, "local") == 0 then
    if proto ~= nil then
      local explist = items[1].items
      local namelist = items[2].items

      local result = items[1].result
      if result < #namelist then
        compiler_error("invalid result", v)
      end
      SR(result - #namelist, "(drop)\n")

      for i = #namelist, 1, -1 do
        local var = namelist[i]
        S"(local.set $" I(var.id) S") (; " S(var.value) S" ;)\n"
      end
    end

  elseif string_compare(kind, "return") == 0 then
    local result = proto.result
    for i = result, 1, -1 do
      S"(local.set $r" I(i) S")\n"
    end
    S"(br $main)\n"

  elseif string_compare(kind, "array") == 0 then
    local result = v.result
    for i = result, 1, -1 do
      S"(local.get $" I(v.id) S")\n"
      S"(i32.const " I(i) S")\n"
      S"(call $" I(ctx.set_index.id) S") (; __set_index ;)\n"
    end

  elseif string_compare(kind, "table") == 0 then
    local keys = {}
    for i = 1, #items do
      local key = items[i].items[1]
      table_insert(keys, key)
    end
    quick_sort(keys, string_compare_value)
    for i = 1, #keys do
      local key = keys[i]
      key.index = i
    end

    for i = #items, 1, -1 do
      local key = items[i].items[1]
      S"(local.get $" I(v.id) S")\n"
      S"(i32.const " I(key.index) S")\n"
      S"(call $" I(ctx.set_index.id) S") (; __set_index ;)\n"
    end

  elseif string_compare(kind, "field") == 0 then
    for i = 2, 1, -1 do
      S"(local.get $" I(v.id) S")\n"
      S"(i32.const " I(i) S")\n"
      S"(call $" I(ctx.set_index.id) S") (; __set_index ;)\n"
    end

  elseif string_compare(kind, "#") == 0 then
    S"(call $" I(ctx.length.id) S") (; __length ;)\n"

  elseif string_compare(kind, "or") == 0 then
    S"end\n"

  elseif string_compare(kind, "and") == 0 then
    S"else\n"
    S"(local.get $dup)\n"
    S"end\n"

  elseif string_compare(kind, "~") == 0 then
    -- bnot or bxor
    if #v.items == 1 then
      S"(i32.const -1)\n"
    end
    S"(i32.xor)\n"

  elseif string_compare(kind, "..") == 0 then
    S"(call $" I(ctx.concat.id) S") (; __concat ;)\n"

  elseif string_compare(kind, "index") == 0 then
    if string_compare(v.resolver, "set") ~= 0 then
      S"(call $" I(ctx.get_index.id) S") (; __get_index ;)\n"
    end

  elseif string_compare(kind, ".") == 0 then
    if string_compare(v.resolver, "set") ~= 0 then
      S"(i32.const " I(items[2].address) S") (; key ;)\n"
      S"(call $" I(ctx.get_table.id) S") (; __get_table ;)\n"
    end
  end
end

function write_function_table(ctx, function_table)
  for i = 1, #function_table do
    local u = function_table[i]
    local items = u.items

    local proto = items[1]
    S"(func $" I(proto.id) S" (; " S(proto.value) S" ;)\n"

    local parlist = items[2].items
    for i = 1, #parlist do
      local par = parlist[i]
      S"(param $" I(par.id) S" i32) (; " S(par.value) S" ;)\n"
    end

    local result = proto.result
    if result > 0 then
      S"(result" SR(result, " i32") S")\n"
    end

    local var_table = proto.ref
    for i = 1, #var_table do
      local var = var_table[i]
      if string_compare(var.resolver, "var") == 0 then
        S"(local $" I(var.id) S" i32) (; " S(var.value) S" ;)\n"
      end
    end
    for i = 1, result do
      S"(local $r" I(i) S" i32)\n"
    end
    S"(local $dup i32)\n"

    S"block $main\n"
    for i = 3, #items do
      process3(ctx, proto, u, items[i])
    end
    if result > 0 then
      S"(unreachable)\n"
    end
    S"end\n"

    for i = 1, result do
      S"(local.get $r" I(i) S")\n"
    end
    S"(return)\n"

    S")\n"
  end
end

function write_proto_table(proto_table)
  -- importした関数も参照を用意する
  local n = 0
  for i = 1, #proto_table do
    local proto = proto_table[i]
    if string_compare(proto.resolver, "fun") == 0 then
      n = n + 1
    end
  end

  S"(table " I(n + 1) S" funcref)\n"
  S"(elem (i32.const 1)"
  for i = 1, #proto_table do
    local proto = proto_table[i]
    if string_compare(proto.resolver, "fun") == 0 then
      S" $" I(proto.id)
    end
  end
  S")\n"
end

function compiler(chunk)
  local ctx = new_ctx()
  local proto_table = {}
  for i = 1, #asm_table do
    local asm = asm_table[i]
    add_asm(ctx, proto_table, new_name(asm[1]), asm[2])
  end

  local var_table = {}
  local scope = new_scope(nil)

  S"(module\n"
  add_wasi(ctx, proto_table, new_name "__fd_prestat_get", 1, "fd_prestat_get", "(param i32 i32)")
  add_wasi(ctx, proto_table, new_name "__fd_prestat_dir_name", 1, "fd_prestat_dir_name", "(param i32 i32 i32)")
  add_wasi(ctx, proto_table, new_name "__path_open", 1, "path_open", "(param i32 i32 i32 i32 i32 i64 i64 i32 i32)")
  add_wasi(ctx, proto_table, new_name "__fd_close", 1, "fd_close", "(param i32)")
  add_wasi(ctx, proto_table, new_name "__fd_read", 1, "fd_read", "(param i32 i32 i32 i32)")
  add_wasi(ctx, proto_table, new_name "__fd_write", 1, "fd_write", "(param i32 i32 i32 i32)")
  add_wasi(ctx, proto_table, new_name "__args_sizes_get", 1, "args_sizes_get", "(param i32 i32)")
  add_wasi(ctx, proto_table, new_name "__args_get", 1, "args_get", "(param i32 i32)")
  add_wasi(ctx, proto_table, new_name "__proc_exit", 0, "proc_exit", "(param i32)")

  local string_tokens = {}
  local function_table = {}
  local chunk_block = chunk.items[1]
  process1(ctx, string_tokens, proto_table, function_table, nil, chunk, chunk_block)

  local string_table, string_end = make_string_table(string_tokens)
  local heap_pointer = roundup(string_end, 1024)
  local memory_size = roundup(heap_pointer, 65536) >> 16
  local heap_pointer_id = add_global(ctx, var_table, scope, new_name "__heap_pointer")

  S"(memory " I(memory_size) S")\n"
  S'(export "memory" (memory 0))\n'

  local result_table = new_result_table(proto_table)
  process2(ctx, proto_table, var_table, result_table, nil, scope, scope, nil, chunk, chunk_block)
  solve_result_table(result_table, function_table)

  ctx.length    = resolve_name(proto_table, scope, new_name "__length")
  ctx.concat    = resolve_name(proto_table, scope, new_name "__concat")
  ctx.new_table = resolve_name(proto_table, scope, new_name "__new_table")
  ctx.set_index = resolve_name(proto_table, scope, new_name "__set_index")
  ctx.get_index = resolve_name(proto_table, scope, new_name "__get_index")
  ctx.set_table = resolve_name(proto_table, scope, new_name "__set_table")
  ctx.get_table = resolve_name(proto_table, scope, new_name "__get_table")

  write_function_table(ctx, function_table)
  write_proto_table(proto_table)

  S"(global $" I(heap_pointer_id) S" (mut i32) (i32.const " I(heap_pointer) S")) (; __heap_pointer ;)\n"

  process3(ctx, nil, chunk, chunk_block)
  write_string_table(string_table)

  S")\n"
end

--------------------------------------------------------------------------------

function lexer_parser(source_file, loaded)
  local result, file = io_open(source_file, "rb")
  if not result then
    local message = file
    error(message)
  end
  local source = file_read_all(file)
  file_close(file)
  return parser(lexer(source_file, source), loaded)
end

function main()
  lexer_initialize()
  parser_initialize()
  compiler_initialize()

  local args = get_args()
  if #args < 1 then
    error "Usage: source_file"
  end

  -- show_memory_usage()
  local chunk = lexer_parser(args[1], {})
  -- show_memory_usage()
  compiler(chunk)
  -- show_memory_usage()
end

__export_start(main)
