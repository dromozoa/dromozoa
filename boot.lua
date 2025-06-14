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
  }
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

  local name = "Name"
  local value = string_sub(source, position, p - 1)

  for i = 1, #lexer_keywords do
    if string_compare(value, lexer_keywords[i]) == 0 then
      name = value
      break
    end
  end

  return p, { name, value }
end

function lexer_rule_symbol(source, position)
  local name = nil
  for i = 2, 1, -1 do
    local symbols = lexer_symbols[i]
    local value = string_sub(source, position, position + i - 1)
    for j = 1, #symbols do
      if string_compare(value, symbols[j]) == 0 then
        return position + i, { value, value }
      end
    end
  end
  return 0, nil
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
      io_write_string "\n"
    end
  end
end

export_start(function ()
  local source = io_read_all()
  lexer(source)
end)
