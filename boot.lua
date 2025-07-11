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

function string_sub(s, i, j)
  local t = {}
  for i = i, j do
    t[#t + 1] = string_byte(s, i, i)
  end
  return string_char(t)
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

function match_char_set(s, p, t)
  local u = string_byte(s, p)
  for i = 1, #t do
    if u == string_byte(t, i) then
      return p + 1
    end
  end
  return 0
end

function match_negative_char_set(s, p, t)
  local u = string_byte(s, p)
  for i = 1, #t do
    if u == string_byte(t, i) then
      return 0
    end
  end
  return p + 1
end

function match_char_range(s, p, t)
  local u = string_byte(s, p)
  for i = 1, #t, 2 do
    if string_byte(t, i) <= u and u <= string_byte(t, i + 1) then
      return p + 1
    end
  end
  return 0
end

function match_repeat(match, s, p, t)
  while true do
    local q = match(s, p, t)
    if q == 0 then
      break
    end
    p = q
  end
  return p
end

function match_search(match, s, p, t)
  local n = #s
  while p <= n do
    local q = match(s, p, t)
    if q ~= 0 then
      return q
    end
    p = p + 1
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
    -- lexer_rule_word;
    -- lexer_rule_number;
    -- lexer_rule_integer;
    -- lexer_rule_string;
    -- lexer_rule_symbol;
  }
end

function lexer_token(lexer, kind, value)
  return {
    kind   = kind;
    value  = value;
    file   = lexer.file;
    line   = lexer.line;
    column = lexer.column;
  }
end

function lexer_error(lexer, message)
  error(lexer.file..":"..integer_to_string(lexer.line)..":"..integer_to_string(lexer.column)..": lexer error: "..message)
end

function lexer_rule_space(lexer, s, p)
  local q = match_repeat(match_char_set, s, p, "\t\n\v\f\r ")
  if q == p then
    return 0, nil
  end
  return q, nil
end

function lexer_rule_comment(lexer, s, p)
  local q = match_literal(s, p, "--")
  if q == 0 then
    return 0, nil
  end
  p = q

  q = match_char_set(s, p, "[")
  if q ~= 0 then
    q = match_repeat(match_char_set, s, q, "=")
    q = match_char_set(s, q, "[")
    if q ~= 0 then
      local t = "]"..string_sub(s, p + 1, q - 2).."]"
      q = match_search(match_literal, s, q, t)
      if q == 0 then
        lexer_error(lexer, "unfinished comment")
      end
      return q, nil
    end
  end

  return match_repeat(match_negative_char_set, s, p, "\n\r"), nil
end

function lexer_rule_word(lexer, s, p)
end

function lexer(file)
  local lexer = {
    file = file;
    line = 1;
    column = 1;
  }

  local tokens = {}

  local s = io_read_file(file)
  local p = 1

  -- 浮動小数点数リテラルは%.%dになりうる

  local n = #s
  while p <= n do
    local q = 0
    local token = nil

    for i = 1, #lexer_rules do
      q, token = lexer_rules[i](lexer, s, p)
      if q ~= 0 then
        break
      end
    end

    if q == 0 then
      lexer_error(lexer, "invalid char")
    end

    local line = lexer.line
    local column = lexer.column
    while p < q do
      if match_literal(s, p, "\r\n") ~= 0 or match_literal(s, p, "\n\r") ~= 0 then
        line = line + 1
        column = 1
        p = p + 2
      elseif match_char_set(s, p, "\n\r") ~= 0 then
        line = line + 1
        column = 1
        p = p + 1
      else
        column = column + 1
        p = p + 1
      end
    end

    lexer.line = line
    lexer.column = column

    p = q
    if token ~= nil then
      tokens[#tokens + 1] = token
    end

  end

  return tokens
end

--------------------------------------------------------------------------------

function parse(parser, source_file)
  lexer(source_file)
end

--------------------------------------------------------------------------------

function main()
  lexer_initialize()

  local parser = {}
  local tree = parse(parser, arg[1])
end

export_start(main)
