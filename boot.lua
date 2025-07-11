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

function match_end_of_line(s, p, t)
  local u = string_byte(s, p)
  if u == 0x0A then
    local v = string_byte(s, p + 1)
    if v == 0x0D then
      return p + 2
    end
    return p + 1
  elseif u == 0x0D then
    local v = string_byte(s, p + 1)
    if v == 0x0A then
      return p + 2
    end
    return p + 1
  end
  return 0
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

function binary_search(t, k, v)
  local i = 1
  local n = #t
  while n > 0 do
    local step = n >> 1
    local m = i + step
    local u = t[m]
    if k > 0 then
      u = u[k]
    end
    local r = string_compare(u, v)
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

function lexer_token(lexer, kind, value)
  return {
    kind   = kind;
    value  = value;
    file   = lexer.file;
    line   = lexer.line;
    column = lexer.column;
  }
end

function lexer_update(lexer, position)
  while lexer.position < position do
    local p = match_end_of_line(lexer.source, lexer.position, nil)
    if p == 0 then
      lexer.position = lexer.position + 1
      lexer.column = lexer.column + 1
    else
      lexer.position = p
      lexer.line = lexer.line + 1
      lexer.column = 1
    end
  end
end

function lexer_error(lexer)
  error(lexer.file..":"..integer_to_string(lexer.line)..":"..integer_to_string(lexer.column)..": lexer error")
end

function lexer_rule_space(lexer, source, position)
  local p = match_repeat(match_char_set, source, position, "\t\n\v\f\r ")
  if p == position then
    return 0, nil
  end
  lexer_update(lexer, p)
  return p, nil
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
        lexer_error(lexer)
      end
      lexer_update(lexer, q)
      return q, nil
    end
  end

  local q = match_repeat(match_negative_char_set, s, p, "\n\r")
  lexer_update(lexer, q)
  return q, nil
end

function lexer_rule_word(lexer, s, p)
  local q = match_char_range(s, p, "AZaz__")
  if q == 0 then
    return 0, nil
  end
  q = match_repeat(match_char_range, s, q, "09AZaz__")

  local v = string_sub(s, p, q - 1)
  local i = binary_search(lexer_keywords, 0, v)
  local token = nil
  if i == 0 then
    token = lexer_token(lexer, "Name", v)
  else
    token = lexer_token(lexer, v, v)
  end
  lexer_update(lexer, q)
  return q, token
end

function lexer_rule_number()
  -- 浮動小数点数リテラルは%.%dになりうる
end

function lexer_rule_string(lexer, s, p)
  local q = match_char_set(s, p, "\"\'")
  if q == 0 then
    return 0, nil
  end
  local quote = string_sub(s, p, p)

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

--[[
  local tokens = {}
  while lexer.position <= #lexer.source do
    local p = 0
    local token = nil

    for i = 1, #lexer_rules do
      p, token = lexer_rules[i](lexer)
      if p ~= 0 then
        break
      end
    end


  end
]]

  local s = io_read_file(file)
  local p = 1
  local n = #s
  local tokens = {}

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
      lexer_error(lexer)
    end

    -- lexer.position = p
    -- lexer_update(lexer, q)
    p = lexer.position
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
