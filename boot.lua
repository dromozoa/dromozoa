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

  local symbols = {
    "+";
    "-";
    "*";
    "/";
    "%";
    "^";
    "#";
    "&";
    "~";
    "|";
    "<<";
    ">>";
    "//";
    "==";
    "~=";
    "<=";
    ">=";
    "<";
    ">";
    "=";
    "(";
    ")";
    "{";
    "}";
    "[";
    "]";
    ";";
    ",";
    "..";
  }

  lexer_symbols = { {}, {} }
  for i = 1, #symbols do
    local symbol = symbols[i]
    table_insert(lexer_symbols[#symbol], symbol)
  end

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

  if string_sub(source, p, p + 1) ~= "--" then
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

  local u = "Name"
  local v = string_sub(source, position, p - 1)

  for i = 1, #lexer_keywords do
    if string_compare(v, lexer_keywords[i]) == 0 then
      u = v
      break
    end
  end

  return p, { u, v, position }
end

function lexer_rule_symbol(source, position)
  for i = 2, 1, -1 do
    local symbols = lexer_symbols[i]
    local v = string_sub(source, position, position + i - 1)
    for j = 1, #symbols do
      if string_compare(v, symbols[j]) == 0 then
        return position + i, { v, v, position }
      end
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
  if prefix == "0X" or prefix == "0x" then
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
      -- token
      io_write_string(token[1])
      io_write_string "\t"
      io_write_string(token[2])
      io_write_string "\t"
      io_write_string(token[3])
      io_write_string "\n"
    end
  end
end

export_start(function ()
  local source = io_read_all()
  lexer(source)
end)
