#! /usr/bin/env lua

local function dump(out, value, depth)
  local t = type(value)
  if t == "number" then
    out:write(("%.17g"):format(value))
    return out
  elseif t == "string" then
    out:write(("%q"):format(value))
    return out
  elseif t == "boolean" then
    if value then
      out:write "true"
    else
      out:write "false"
    end
    return out
  end
  assert(t == "table")

  local keys = {}
  for k, v in pairs(value) do
    if type(k) == "string" then
      keys[#keys + 1] = k
    end
  end
  table.sort(keys)

  local m = #keys
  local n = #value

  if m + n == 0 then
    out:write "{}"
    return
  end

  depth = depth or 0
  local indent = ("  "):rep(depth)
  depth = depth + 1

  out:write "{"
  for i = 1, m do
    local k = keys[i]
    out:write("\n", indent, ("  [%q] = "):format(k))
    dump(out, value[k], depth)
    out:write ";"
  end
  for i = 1, n do
    out:write("\n", indent, "  ")
    dump(out, value[i], depth)
    out:write ";"
  end
  out:write("\n", indent, "}")

  return out
end

local function lexer(source)
  local rules = {
    { skip = true, pattern = "%s+" };
    { skip = true, pattern = "%-%-[^\r\n]*" };

    "end";
    "function";
    "local";

    "+";
    "-";
    "*";
    "/";
    "%";
    "=";
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

local function parser(tokens)
  local index = 1

  local function parser_error(token)
    error("parser error at token <"..token.name.."> position "..token.i)
  end

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
      parser_error(token)
    end
    next_token()
    return token
  end

  local parse_expression
  local parse_statement

  local function parse_names(tag, separator, close)
    local result = { tag = tag }

    while true do
      if peek_token().name ~= "Name" then
        break
      end
      result[#result + 1] = next_token()
      expect_token(separator)
    end

    if close then
      expect_token(close)
    end

    return result
  end

  local function parse_expressions(tag, separator, close)
    local result = { tag = tag }

    local token = peek_token()
    if token.name == close then
      next_token()
    else
      while true do
        result[#result + 1] = parse_expression(0)

        local token = peek_token()
        if token.name == close then
          next_token()
          break
        end
        expect_token(separator)
      end
    end

    return result
  end

  local NUD = {} -- null denotion
  local LBP = {} -- left binding power
  local LED = {} -- left denotion

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

  local function postfix_call(bp)
    postfix("(", bp, function (token, node)
      return { tag = "call", token, node, parse_expressions("args", ",", ")") }
    end)
  end

  local bp = 0

  prefix "Integer"
  prefix "Name"
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
  postfix_call(bp)

  local function parse_block()
    local block = { tag = "block" }

    while true do
      local statement = parse_statement()
      if not statement then
        break
      end
      block[#block + 1] = statement
    end

    return block
  end

  function parse_statement()
    local token = peek_token()
    if token.name == "function" then
      -- "function" Name funcbody end
      next_token()

      local token = expect_token "Name"
      expect_token "("
      local params = parse_names("params", ",", ")")
      local block = parse_block()
      expect_token "end"

      return { tag = "function", token, params, block }

    elseif token.name == "Name" then

      local vars = parse_names("vars", ",", "=")
    end
  end

  function parse_expression(rbp)
    local token = next_token()
    local nud = NUD[token.name]
    if not nud then
      parser_error(token)
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

  local result = parse_block()
  -- local result = parse_expression(0)
  local token = peek_token()
  if not token.eof then
    parser_error(token)
  end
  return result
end

local tokens1 = lexer "(12 + 34) * (56 - 78)"
local tokens2 = lexer "-4 - -x"
local tokens3 = lexer "f() + g(1 + 1) + h(1, 2, 3 * 4)"
local tokens4 = lexer [[
function f1()
end
]]

local tokens = tokens4
-- dump(io.stdout, tokens):write "\n"

local result = parser(tokens)
dump(io.stdout, result):write "\n"
