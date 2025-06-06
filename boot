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
        if not rule.skip then
          tokens[#tokens + 1] = {
            name = assert(rule.name);
            value = v;
            i = i;
            j = j;
          }
        end
        break
      end
    end

    if not i then
      error("lexer error at position "..position)
    end
    position = j + 1
  end

  tokens[#tokens + 1] = {
    name = "EOF";
    eof = true;
  }

  return tokens
end

-- 最小限のPrattパーサを書いてみる。
local function parser(tokens)
  local NUD = {} -- null denotion
  local LBP = {} -- left binding power
  local LED = {} -- left denotion

  local parse_expression

  local function prefix(name, nud)
    NUD[name] = nud or function (token)
      return token
    end
  end

  local function prefix_operator(name, bp)
    prefix(name, function (token)
      return { token, parse_expression(bp) }
    end)
  end

  local function infix(name, bp, led)
    LBP[name] = bp
    LED[name] = led or function (token, node)
        return { token, node, parse_expression(bp) }
    end
  end

  local function infix_right(name, bp)
    infix(name, bp, function (token, node)
      return { token, node, parse_expression(bp - 1) }
    end)
  end

  local function postfix(name, bp, led)
    LBP[name] = bp
    LED[name] = led or function (token, node)
      return { token, node }
    end
  end

  prefix("Integer")
  prefix("Name")

  infix("+", 10)
  infix("-", 10)
  infix("*", 20)
  prefix_operator("-", 200)

  local index = 1

  local function peek_token()
    return tokens[index]
  end

  local function next_token()
    local token = peek_token()
    index = index + 1
    return token
  end

  function parse_expression(rbp)
    local token = next_token()
    local nud = assert(NUD[token.name])
    local node = nud(token)

    while true do
      local token = peek_token()

      local lbp = LBP[token.name]
      if not lbp or lbp <= rbp then
        break
      end

      local led = LED[token.name]
      next_token()
      node = led(token, node)
    end

    return node
  end

  local result = parse_expression(0)
  assert(peek_token().eof)
  return result
end

local tokens1 = lexer "12 + 34 * 56 - 78"
local tokens2 = lexer "-4 * -5"

local tokens = tokens1
-- print(json.encode(tokens, { pretty = true, stable = true }))

local result = parser(tokens)
print(json.encode(result, { pretty = true, stable = true }))
