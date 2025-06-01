#! /usr/bin/env lua

local json = require "dromozoa.commons.json"

local function lexer(source)
  local rules = {
    { skip = true, pattern = "%s+" };
    { skip = true, pattern = "%-%-[^\r\n]*" };
    "+";
    "-";
    "*";
    "/";
    "%";
    "^"; "?"; -- test
    { name = "Integer", pattern = "%d+" };
    { name = "Name",    pattern = "[%a_][%w_]*" };
  }

  for i, rule in ipairs(rules) do
    if type(rule) == "string" then
      rules[i] = {
        name = rule;
        rule = function (source, position)
          local i = position
          local j = i + #rule - 1
          if source:sub(i, j) == rule then
            return i, j
          end
        end;
      }
    else
      local pattern = "^"..rule.pattern
      rule.rule = function (source, position)
        return source:find(pattern, position)
      end
    end
  end

  local tokens = {}

  local position = 1
  while position <= #source do
    local i, j, v
    for _, rule in ipairs(rules) do
      i, j, v = rule.rule(source, position)
      if i then
        if not v then
          v = source:sub(i, j)
        end
        tokens[#tokens + 1] = {
          name = rule.name;
          skip = rule.skip;
          value = v;
          i = i;
          j = j;
        }
        break
      end
    end

    if not i then
      error("lexer error at position "..position)
    end
    position = j + 1
  end

  return tokens
end

-- bp = binding power
-- 最小限のPrattパーサを書いてみる。
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

  local function postfix(symbol, bp)
    LED[symbol] = {
      lbp = bp;
      rbp = bp;
      action = function (left, token)
        return { symbol, left }
      end;
    }
  end

  local function prefix_unary(symbol, bp)
    NUD[symbol] = function (symbol)
      local right = parse_expression(bp)
      return { symbol, right }
    end
  end

  prefix("INTEGER")
  infix("+", 10)
  infix("-", 10)
  infix("*", 20)
  infix_r("^", 100)
  prefix_unary("-", 200)
  postfix("?", 300)

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

local tokens1 = lexer "12 + 34 * 56 - 78"
local tokens2 = lexer "2^3^2"
local tokens3 = lexer "-4--5?"

-- local tokens = lexer "12+34*56-78"
local tokens1 = {
  { "INTEGER", 12 },
  { "+" },
  { "INTEGER", 34 },
  { "*" },
  { "INTEGER", 56 },
  { "-" },
  { "INTEGER", 78 },
  { "eof" },
}

local tokens2 = {
  { "INTEGER", 2 },
  { "^" },
  { "INTEGER", 3 },
  { "^" },
  { "INTEGER", 2 },
  { "eof" },
}

local tokens3 = {
  { "-" },
  { "INTEGER", 4 },
  { "-" },
  { "-" },
  { "INTEGER", 5 },
  { "?" },
  { "eof" },
}

local result = parser(tokens3)
print(json.encode(result, { pretty = true }))

-- local tokens = lexer [[
-- 12 + 34 * 56
-- --test
-- -78
-- ]]
-- print(json.encode(tokens, { pretty = true, stable = true }))
