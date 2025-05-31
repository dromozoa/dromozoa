#! /usr/bin/env lua

local json = require "dromozoa.commons.json"

-- bp = binding power
-- 最小限のPrattパーサを書いてみる。
-- まずは式だけとして、文はどうするのがよいか。
local function parser(tokens)

  local NUD = {}
  local LED = {}

  local parse_expression

  local function prefix(symbol)
    NUD[symbol] = function (symbol)
      return symbol
    end
  end

  local function infix(symbol, bp, action)
    LED[symbol] = {
      lbp = bp;
      rbp = bp;
      action = function (left, token)
        local right = parse_expression(bp)
        return { symbol, left, right }
      end;
    }
  end

  local function infix_r(symbol, bp, action)
    LED[symbol] = {
      lbp = bp;
      rbp = bp - 1;
      action = function (left, token)
        local right = parse_expression(bp - 1)
        return { symbol, left, right }
      end;
    }
  end

  prefix "INTEGER"
  infix("+", 10)
  infix("-", 10)
  infix("*", 20)

  local index = 1

  local function peek()
    return tokens[index]
  end

  local function next()
    local token = peek()
    index = index + 1
    return token
  end

  function parse_expression(rbp)
    local token = next()
    local nud = assert(NUD[token[1]])
    local left = nud(token)

    while true do
      local look = peek()
      local inf = LED[look[1]]
      if not inf or inf.lbp <= rbp then
        break
      end
      local op_token = next()
      left = inf.action(left, op_token)
    end

    return left
  end

  return parse_expression(0)
end

-- local tokens = lexer "12+34*56-78"
local tokens = {
  { "INTEGER", 12 },
  { "+" },
  { "INTEGER", 34 },
  { "*" },
  { "INTEGER", 56 },
  { "-" },
  { "INTEGER", 78 },
  { "eof" },
}
local result = parser(tokens)
print(json.encode(result, { pretty = true }))
