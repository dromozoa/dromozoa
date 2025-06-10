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

local function short_string(source, position)
  local i, j, q = source:find([[^(["'])]], position)
  if not i then
    return
  end
  local k, l = source:find("[^\\]"..q, position)
  if not k then
    error("lexer error at position "..position)
  end
  local v = source:sub(i + 1, k)
    :gsub([[\([abfnrtv\"'])]], {
      a = "\a", b = "\b", f = "\f", n = "\n", r = "\r", t = "\t", v = "\v";
      ["\\"] = "\\";
      ['"'] = '"';
      ["'"] = "'";
    })
    :gsub([[\z%s*]], "")
    :gsub([[\x(%x%x)]], function (v)
      return string.char(tonumber(v, 16))
    end)
    :gsub([[\(%d%d?%d?)]], function (v)
      return string.char(tonumber(v, 10))
    end)
    :gsub([[\u{(%x+)}]], function (v)
      return utf8.char(tonumber(v, 16))
    end)
  return i, l, v
end

local function lexer(source)
  local rules = {
    { skip = true, pattern = "%s+" };
    { skip = true, pattern = "%-%-[^\r\n]*" };

    "break";
    "do";
    "elseif";
    "else";
    "end";
    "function";
    "if";
    "local";
    "return";
    "then";
    "while";

    "+";
    "-";
    "*";
    "/";
    "%";
    "=";
    "(";
    ")";
    ";";
    ",";

    { name = "Name",    pattern = "[%a_][%w_]*" };
    { name = "Integer", pattern = "%d+" };
    { name = "String",  rule = short_string };
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
    elseif rule.pattern then
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

  local function read_token()
    local token = tokens[index]
    index = index + 1
    return token
  end

  local function unread_token()
    index = index - 1
  end

  local function expect_token(name)
    local token = tokens[index]
    if token.name ~= name then
      parser_error(token)
    end
    index = index + 1
    return token
  end

  local parse_expression
  local parse_statement

  local function parse_items(tag, separator, close, parse)
    local result = { tag = tag }

    while true do
      local item = parse()
      if not item then
        break
      end
      result[#result + 1] = item

      if separator then
        if peek_token().name ~= separator then
          break
        end
        read_token()
      end
    end

    if close then
      expect_token(close)
    end

    return result
  end

  local function parse_names(tag, separator, close)
    return parse_items(tag, separator, close, function ()
      if peek_token().name == "Name" then
        return read_token()
      end
    end)
  end

  local function parse_expressions(tag, separator, close)
    return parse_items(tag, separator, close, function ()
      return parse_expression(0, true)
    end)
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
      return { tag = "call", node, parse_expressions("arguments", ",", ")") }
    end)
  end

  local bp = 0

  prefix "Name"
  prefix "Integer"
  prefix "String"
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
    return parse_items("block", nil, nil, parse_statement)
  end

  local function parse_function(tag)
    local token = expect_token "Name"
    expect_token "("
    local parameters = parse_names("parameters", ",", ")")
    local block = parse_block()
    expect_token "end"

    return { tag = tag, token, parameters, block }
  end

  local function parse_if(tag)
    local expression = parse_expression(0)
    expect_token "then"
    local block = parse_block()
    local result = { tag = tag, expression, block }

    local token = read_token()
    if token.name == "else" then
      local block = parse_block()
      expect_token "end"
      result[#result + 1] = block
      return result

    elseif token.name == "elseif" then
      result[#result + 1] = parse_if "elseif"
      return result

    elseif token.name == "end" then
      return result

    else
      unread_token()
      parse_error(token)
    end
  end

  local function parse_call(tag)
    local result

    local token = read_token()
    if token.name == "Name" then
      if peek_token().name == "(" then
        read_token()
        result = { tag = tag, token, parse_expressions("arguments", ",", ")") }
      end
    elseif token.name == "(" then
      unread_token()
      result = parse_expression(0)
    end

    if not result then
      unread_token()
      return
    end

    while peek_token().name == "(" do
      read_token()
      result = { tag = tag, result, parse_expressions("arguments", ",", ")") }
    end

    return result
  end

  function parse_statement()
    local call = parse_call "call"
    if call then
      return call
    end

    local token = read_token()

    if token.name == ";" then
      return { tag = ";" }

    elseif token.name == "Name" then
      unread_token()
      local variables = parse_names("variables", ",", "=")
      local expressions = parse_expressions("expressions", ",")

      return { tag = "assign", variables, expressions }

    elseif token.name == "break" then
      return { tag = "break" }

    elseif token.name == "do" then
      local block = parse_block()
      expect_token "end"

      return { tag = "do", block }

    elseif token.name == "while" then
      local expression = parse_expression(0)
      expect_token "do"
      local block = parse_block()
      expect_token "end"

      return { tag = "while", expression, block }

    elseif token.name == "function" then
      return parse_function "function"

    elseif token.name == "if" then
      return parse_if "if"

    elseif token.name == "local" then
      local token = peek_token()
      if token.name == "function" then
        read_token()
        return parse_function "local function"
      elseif token.name == "Name" then
        local variables = parse_names("variables", ",")
        local expressions
        if peek_token().name == "=" then
          read_token()
          expressions = parse_expressions("expressions", ",")
        end
        return { tag = "local", variables, expressions }
      else
        parser_error(token)
      end

    elseif token.name == "return" then
      local expressions = parse_expressions("expressions", ",")

      return { tag = "return", expressions }

    else
      unread_token()
    end
  end

  function parse_expression(rbp, return_if_not_nud)
    local token = read_token()
    local nud = NUD[token.name]
    if not nud then
      if return_if_not_nud then
        unread_token()
        return
      else
        parser_error(token)
      end
    end
    local node = nud(token)

    while true do
      local token = peek_token()

      local lbp = LBP[token.name]
      if not lbp or lbp <= rbp then
        break
      end

      local led = LED[token.name]
      read_token()
      node = led(token, node)
    end

    return node
  end

  local result = parse_block()
  local token = peek_token()
  if not token.eof then
    parser_error(token)
  end
  return result
end

local function compiler(tree)
end

local source = io.read "*a"
-- dump(io.stdout, tokens):write "\n"
local tokens = lexer(source)
local tree = parser(tokens)
dump(io.stdout, tree):write "\n"
