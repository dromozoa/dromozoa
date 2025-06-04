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
  }

  return tokens
end

-- 最小限のPrattパーサを書いてみる。
local function parser(tokens)
  local BP = {} -- binding power
  local NUD = {} -- null denotion
  local LED = {} -- left denotion

  local parse_expression

  local function prefix(name, action)
    if not action then
      action = function (node)
        return node
      end
    end
    NUD[name] = action
  end

  local function prefix_unary(name, bp, action)
    prefix(name, function (node1)
      local node2 = parse_expression(bp)
      return { node1, node2 }
    end)
  end

  local function infix(name, bp, action)
    if not action then
      action = function (node1, node2, _)
        local node3 = parse_expression(bp)
        return { node1, node2, node3 }
      end
    end
    BP[name] = bp
    LED[name] = action
  end

  local function infix_r(name, bp, action)
    infix(name, bp, function (node1, node2, _)
      local node3 = parse_expression(bp - 1)
      return { node1, node2, node3 }
    end)
  end

  local function postfix(name, bp, action)
    if not action then
      action = function (node1, node2, _)
        return { node1, node2 }
      end
    end
    BP[name] = bp
    LED[name] = action
  end

  prefix("Integer")
  infix("+", 10)
  infix("-", 10)
  infix("*", 20)
  prefix_unary("-", 200)

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
    local nud = assert(NUD[token.name])
    local left = nud(token)

    while true do
      local look = peek()
      local lbp = BP[look.name]

      if not lbp or lbp <= rbp then
        break
      end
      local led = LED[look.name]
      local op_token = next()
      left = led(look, left, op_token)
    end

    return left
  end

  return parse_expression(0)
end

local tokens1 = lexer "12 + 34 * 56 - 78"
-- local tokens2 = lexer "2^3^2"
local tokens3 = lexer "-4- -5"

local tokens = tokens3
-- print(json.encode(tokens, { pretty = true, stable = true }))

local result = parser(tokens)
print(json.encode(result, { pretty = true }))
