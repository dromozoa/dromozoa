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
    "(";
    ")";
    ",";

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
    i = position;
    j = position;
  }

  return tokens
end

-- 最小限のPrattパーサを書いてみる。
local function parser(tokens)
  local index = 1

  local function peek_token()
    return tokens[index]
  end

  local function next_token()
    local token = peek_token()
    index = index + 1
    return token
  end

  local function expect_token(name)
    local token = peek_token()
    if token.name ~= name then
      error("parser error at token <"..token.name.."> position "..token.i)
    end
    next_token()
  end

  local NUD = {} -- null denotion
  local LBP = {} -- left binding power
  local LED = {} -- left denotion

  local parse_expression

  local function prefix(name, nud)
    NUD[name] = nud or function (token)
      return token
    end
  end

  local function prefix_group(open, close)
    prefix(open, function (token)
      local group = parse_expression(0)
      expect_token(close)
      return group
    end)
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

  local function postfix_call(open, close, separator, bp)
    postfix(open, bp, function (callee_token, node)
      local arguments = {}

      local token = peek_token()
      if token.name == close then
        next_token()
      else
        while true do
          arguments[#arguments + 1] = parse_expression(0)

          local token = peek_token()
          if token.name == close then
            next_token()
            break
          end
          expect_token(separator)
        end
      end

      return { callee_token, node, arguments }
    end)
  end

  local bp = 0

  prefix("Integer")
  prefix("Name")
  prefix_group("(", ")")

  bp = bp + 10
  infix("+", bp)
  infix("-", bp)

  bp = bp + 10
  infix("*", bp)
  infix("/", bp)
  infix("%", bp)

  bp = bp + 10
  prefix_operator("-", bp)

  bp = bp + 10
  postfix_call("(", ")", ",", bp)

  function parse_expression(rbp)
    local token = next_token()
    local nud = NUD[token.name]
    if not nud then
      error("parser error at token <"..token.name.."> position "..token.i)
    end
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
  local token = peek_token()
  if not token.eof then
    error("parser error at token <"..token.name.."> position "..token.i)
  end
  return result
end

local tokens1 = lexer "(12 + 34) * (56 - 78)"
local tokens2 = lexer "-4 - -x"
local tokens3 = lexer "f() + g(1 + 1) + h(1, 2, 3 * 4)"

local tokens = tokens3
-- print(json.encode(tokens, { pretty = true, stable = true }))

local result = parser(tokens)
print(json.encode(result, { pretty = true, stable = true }))
