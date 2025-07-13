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
local json = require "dromozoa.commons.json"

--------------------------------------------------------------------------------

function string_compare(a, b)
  local n = #a
  if n > #b then
    n = #b
  end
  for i = 1, n + 1 do
    local u = string_byte(a, i)
    local v = string_byte(b, i)
    if u ~= v then
      return u - v
    end
  end
  return 0
end

--------------------------------------------------------------------------------

function match_literal(s, p, t)
  for i = 1, #t do
    local u = string_byte(s, p)
    local v = string_byte(t, i)
    if u ~= v then
      return 0
    end
    p = p + 1
  end
  return p
end

function match_search(s, p, t, c)
  local n = #s
  while p <= n do
    local q = match_literal(s, p, t)
    if q ~= 0 then
      return q
    end
    if c ~= nil then
      c[#c + 1] = string_byte(s, p)
    end
    p = p + 1
  end
  return 0
end

--------------------------------------------------------------------------------

function match_charset(s, p, t, c)
  local u = string_byte(s, p)
  for i = 1, #t do
    if u == string_byte(t, i) then
      if c ~= nil then
        c[#c + 1] = u
      end
      return p + 1
    end
  end
  return 0
end

function match_nagative_charset(s, p, t, c)
  local u = string_byte(s, p)
  for i = 1, #t do
    if u == string_byte(t, i) then
      return 0
    end
  end
  if c ~= nil then
    c[#c + 1] = u
  end
  return p + 1
end

function match_range(s, p, t, c)
  local u = string_byte(s, p)
  for i = 1, #t, 2 do
    if string_byte(t, i) <= u and u <= string_byte(t, i + 1) then
      if c ~= nil then
        c[#c + 1] = u
      end
      return p + 1
    end
  end
  return 0
end

function match_repeat(match, s, p, t, c)
  while true do
    local q = match(s, p, t, c)
    if q == 0 then
      break
    end
    p = q
  end
  return p
end

function binary_search(t, v)
  local i = 1
  local n = #t
  while n > 0 do
    local step = n >> 1
    local m = i + step
    local r = string_compare(t[m], v)
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

function new_info(file, position, line, column)
end

function new_token(kind, value)
  return { kind = kind, value = value }
end

--------------------------------------------------------------------------------

local lexer_keywords
local lexer_symbols
local lexer_rules

function lexer_initialize()
  lexer_keywords = {
    "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "goto", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while"
  }

  lexer_symbols = {
    { "#", "%", "&", "(", ")", "*", "+", ",", "-", ".", "/", ":", ";", "<", "=", ">", "[", "]", "^", "{", "|", "}", "~" };
    { "..", "//", "::", "<<", "<=", "==", ">=", ">>", "~=" };
    { "..." };
  }

  lexer_rules = {
    lexer_rule_space;
    lexer_rule_comment;
    lexer_rule_word;
    -- lexer_rule_number;
    -- lexer_rule_integer;
    -- lexer_rule_string;
    -- lexer_rule_symbol;
  }
end

function lexer_update(lexer, position)
  while lexer.position < position do
    local u = string_byte(lexer.source, lexer.position)
    lexer.position = lexer.position + 1
    if u == 0x0A or u == 0x0D then
      if string_byte(lexer.source, lexer.position) == 0x17 - u then
        lexer.position = lexer.position + 1
      end
      lexer.line = lexer.line + 1
      lexer.column = 1
    else
      lexer.column = lexer.column + 1
    end
  end
end

function lexer_token(lexer, kind, value, position)
  local token = {
    kind     = kind;
    value    = value;
    file     = lexer.file;
    position = lexer.position;
    line     = lexer.line;
    column   = lexer.column;
  }
  lexer_update(lexer, position)
  return token
end

function lexer_error(lexer)
  error(lexer.file..":"..integer_to_string(lexer.line)..":"..integer_to_string(lexer.column)..": lexer error")
end

function lexer_rule_space(lexer, source, position)
  local capture = {}
  local p = match_repeat(match_charset, source, position, "\t\n\v\f\r ", capture)
  if p == position then
    return nil
  end
  return lexer_token(lexer, "space", string_char(capture), p)
end

function lexer_rule_comment(lexer, s, p)
  local q = match_literal(s, p, "--")
  if q == 0 then
    return nil
  end
  p = q

  q = match_charset(s, p, "[")
  if q ~= 0 then
    local capture = {}
    q = match_repeat(match_charset, s, q, "=", capture)
    q = match_charset(s, q, "[")
    if q ~= 0 then
      local t = "]"..string_char(capture).."]"
      local capture = {}
      q = match_search(s, q, t, capture)
      if q == 0 then
        lexer_error(lexer)
      end
      return lexer_token(lexer, "comment", string_char(capture), q)
    end
  end

  local capture = {}
  local q = match_repeat(match_nagative_charset, s, p, "\n\r", capture)
  return lexer_token(lexer, "comment", string_char(capture), q)
end

function lexer_rule_word(lexer, s, p)
  local capture = {}
  local q = match_range(s, p, "AZaz__", capture)
  if q == 0 then
    return nil
  end
  q = match_repeat(match_range, s, q, "09AZaz__", capture)

  local v = string_char(capture)
  local i = binary_search(lexer_keywords, v)
  local token = nil
  if i == 0 then
    token = lexer_token(lexer, "Name", v, q)
  else
    token = lexer_token(lexer, v, v, q)
  end
  return token
end

function lexer_rule_number()
  -- 浮動小数点数リテラルは%.%dになりうる
end

function lexer_rule_string(lexer, s, p)
  local capture = {}
  local q = match_charset(s, p, "\"\'", capture)
  if q == 0 then
    return 0, nil
  end
  local quote = string_char(capture)

  return 0, nil
end

function lexer(file)
  local lexer = {
    file     = file;
    source   = io_read_file(file);
    position = 1;
    line     = 1;
    column   = 1;
  }

  local s = io_read_file(file)
  local n = #s
  local tokens = {}

  while lexer.position <= n do
    local token = nil

    for i = 1, #lexer_rules do
      token = lexer_rules[i](lexer, s, lexer.position)
      if token ~= nil then
        break
      end
    end

    if token == nil then
      lexer_error(lexer)
    end

    if token ~= nil then
      tokens[#tokens + 1] = token
    end
  end

  return tokens
end

--------------------------------------------------------------------------------

function parse(parser, source_file)
  local tokens = lexer(source_file)
  print(json.encode(tokens, { pretty = true, stable = true }))
end

--------------------------------------------------------------------------------

function main()
  lexer_initialize()

  local parser = {}
  local tree = parse(parser, arg[1])
end

export_start(main)
