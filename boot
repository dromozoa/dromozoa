#! /usr/bin/env lua

-- アイディア: 関数式も関数ポインタに変換できる
-- アイディア: テーブルコンストラクタは少なくとも配列に変換できる

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
    "false";
    "function";
    "if";
    "local";
    "return";
    "then";
    "true";
    "while";

    "+";
    "-";
    "*";
    "/";
    "%";
    "==";
    "~=";
    "<";

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
    local token
    for _, rule in ipairs(rules) do
      i, j, v = rule.rule(source, position)
      if i then
        if not v then
          v = source:sub(i, j)
        end
        if not token or token.j < j then
          token = {
            name = rule.name;
            value = v;
            i = i;
            j = j;
            skip = rule.skip
          }
        end
      end
    end
    if not token then
      error("lexer error at position "..position)
    end

    if not token.skip then
      tokens[#tokens + 1] = token
    end
    position = token.j + 1
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

  local function parse_items(name, separator, close, parse)
    local result = { name = name }

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

  local function parse_names(name, separator, close, not_ref, def)
    local result = parse_items(name, separator, close, function ()
      if peek_token().name == "Name" then
        return read_token()
      end
    end)
    for _, name in ipairs(result) do
      name.not_ref = not_ref
      name.def = def
    end
    return result
  end

  local function parse_expressions(name, separator, close)
    return parse_items(name, separator, close, function ()
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
      return { type = "prefix", name = token.name, parse_expression(bp) }
    end)
  end

  local function infix(name, bp, led)
    LBP[name] = bp
    LED[name] = led or function (token, node)
        return { type = "left", name = token.name, node, parse_expression(bp) }
    end
  end

  local function infix_right(name, bp)
    infix(name, bp, function (token, node)
      return { type = "right", name = token.name, node, parse_expression(bp - 1) }
    end)
  end

  local function postfix(name, bp, led)
    LBP[name] = bp
    LED[name] = led or function (token, node)
      return { type = "postfix", name = token.name, node }
    end
  end

  local function postfix_call(bp)
    postfix("(", bp, function (token, node)
      if node.name == "Name" then
        node.not_ref = true
      end
      return { name = "call", node, parse_expressions("arguments", ",", ")") }
    end)
  end

  local bp = 0

  prefix "true"
  prefix "false"
  prefix "Name"
  prefix "Integer"
  prefix "String"
  prefix_group("(", ")")

  bp = bp + 10
  infix("==", bp)
  infix("~=", bp)
  infix("<", bp)

  bp = bp + 10
  infix("+", bp)
  infix("-", bp)

  bp = bp + 10
  infix("+", bp)
  infix("-", bp)
  infix("*", bp)
  infix("/", bp)
  infix("%", bp)

  bp = bp + 10
  prefix_operator("-", bp)

  bp = bp + 10
  postfix_call(bp)

  local function parse_block(tag)
    local result = parse_items(tag, nil, nil, parse_statement)
    -- repeat-until文がないので単純にブロックにスコープを割り当てる。
    result.scope = {}
    return result
  end

  local function parse_function(name)
    local token = expect_token "Name"
    token.not_ref = true
    expect_token "("
    local parameters = parse_names("parameters", ",", ")", true)
    local block = parse_block "block"
    expect_token "end"
    return { name = name, scope = {}, token, parameters, block }
  end

  local function parse_if(name)
    local expression = parse_expression(0)
    expect_token "then"
    local block = parse_block "block"
    local result = { name = name, expression, block }

    local token = read_token()
    if token.name == "else" then
      local block = parse_block "block"
      expect_token "end"
      result[#result + 1] = block
      return result

    elseif token.name == "elseif" then
      result[#result + 1] = parse_if "if"
      return result

    elseif token.name == "end" then
      return result

    else
      unread_token()
      parse_error(token)
    end
  end

  local function parse_call()
    local result

    local token = read_token()
    if token.name == "Name" then
      if peek_token().name == "(" then
        read_token()
        token.not_ref = true
        result = { name = "call", token, parse_expressions("arguments", ",", ")") }
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
      result = { name = "call", result, parse_expressions("arguments", ",", ")") }
    end

    result.statement = true

    return result
  end

  function parse_statement()
    local call = parse_call()
    if call then
      return call
    end

    local token = read_token()

    if token.name == ";" then
      return { name = ";" }

    elseif token.name == "Name" then
      unread_token()
      local variables = parse_names("variables", ",", "=", true, true)
      local expressions = parse_expressions("expressions", ",")
      return { name = "assign", variables, expressions }

    elseif token.name == "break" then
      return { name = "break" }

    elseif token.name == "do" then
      local block = parse_block "block"
      expect_token "end"
      return { name = "do", block }

    elseif token.name == "while" then
      local expression = parse_expression(0)
      expect_token "do"
      local block = parse_block "block"
      expect_token "end"
      return { name = "while", expression, block }

    elseif token.name == "function" then
      return parse_function "function"

    elseif token.name == "if" then
      return parse_if "if"

    elseif token.name == "local" then
      local token = peek_token()
      if token.name == "function" then
        read_token()
        local result = parse_function "function"
        result["local"] = true
        return result
      elseif token.name == "Name" then
        local variables = parse_names("variables", ",", nil, true)
        local expressions
        if peek_token().name == "=" then
          read_token()
          expressions = parse_expressions("expressions", ",")
        end
        return { name = "local", variables, expressions }
      else
        parser_error(token)
      end

    elseif token.name == "return" then
      local expressions = parse_expressions("expressions", ",")
      return { name = "return", expressions }

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

  local result = parse_block "chunk"
  local token = peek_token()
  if not token.eof then
    parser_error(token)
  end
  return result
end

local function compiler(chunk)
  local identifier = 0
  local strings = {}
  local string_offset = 1024

  local function make_identifier()
    identifier = identifier + 1
    return "$"..identifier
  end

  local instruction_table = {}
  local function_table = {}
  local external_scope = { type = "external", names = {} }

  local function add_external_scope_instruction(name, instruction)
    local id = make_identifier()
    local names = external_scope.names
    local def = { name = { id = id, value = name }, type = "instruction" }
    names[#names + 1] = def
    external_scope[name] = def
    instruction_table[id] = instruction
  end

  local function add_external_scope_import_function(name, result)
    local id = make_identifier()
    local names = external_scope.names
    local def = { name = { id = id, value = name, result = result }, type = "function" }
    names[#names + 1] = def
    external_scope[name] = def
    function_table[id] = def.name
  end

  local function add_external_scope_variable(name)
    local id = make_identifier()
    local names = external_scope.names
    local def = { name = { id = id, value = name }, type = "variable" }
    names[#names + 1] = def
    external_scope[name] = def
  end

  local function gen_call_indirect(m)
    return function (u)
      local n = #u[2] - 1
      io.write "(call_indirect"
      if n > 0 then
        io.write " (param"
        for i = 1, n do
          io.write " i32"
        end
        io.write ")"
      end
      if m > 0 then
        io.write " (result"
        for i = 1, m do
          io.write " i32"
        end
        io.write ")"
      end
      io.write ")\n"
    end
  end

  add_external_scope_instruction("i32_load", "(i32.load)")
  add_external_scope_instruction("i32_store", "(i32.store)")
  add_external_scope_instruction("i32_store8", "(i32.store8)")
  for i = 0, 8 do
    add_external_scope_instruction("call_indirect"..i, gen_call_indirect(i))
  end
  add_external_scope_import_function("fd_write", 1)
  add_external_scope_variable "stack_pointer"
  add_external_scope_variable "stack_offset"
  add_external_scope_variable "heap_pointer"
  add_external_scope_variable "heap_offset"

  local function make_alignment(n, a)
    local r = n % a
    if r == 0 then
      return n
    else
      return n + a - r
    end
  end

  local function get_names(scope)
    while true do
      if scope.names then
        return scope.names
      end
      scope = scope.parent
    end
  end

  local function get_results(scope)
    while true do
      if scope.results then
        return scope.results
      end
      scope = scope.parent
    end
  end

  local function find_name(scope, name)
    while true do
      local def = scope[name]
      if def then
        assert(def.name, name)
        return def, scope.type
      end

      if not scope.parent then
        return nil
      end

      scope = scope.parent
    end
  end

  local function visit(u, scope, loop)
    if u.scope then
      u.scope.parent = scope
      if u.name == "chunk" or u.name == "function" then
        u.scope.type = u.name
        u.scope.names = {}
        u.scope.results = {}
      elseif scope then
        u.scope.type = scope.type
      end
      scope = u.scope
    end

    if u.name == "call" then
      assert(u[1].name == "Name")
      local def, type = assert(find_name(scope, u[1].value))
      u[1].id = def.name.id

    elseif u.name == "break" then
      assert(loop)
      u.loop = loop

    elseif u.name == "while" then
      loop = u
      u.block_id = make_identifier()
      u.loop_id = make_identifier()

    elseif u.name == "function" then
      loop = nil

      -- チャンクスコープに関数名を登録する。
      local names = get_names(scope.parent)
      local fname = u[1]
      local def = {
        name = fname;
        type = "function";
      }
      names[#names + 1] = def
      fname.id = make_identifier()
      scope.parent[fname.value] = def
      function_table[fname.id] = fname

      local names = scope.names
      for _, parameter in ipairs(u[2]) do
        local def = {
          name = parameter;
          type = "parameter";
        }
        names[#names + 1] = def
        parameter.id = make_identifier()
        scope[parameter.value] = def
      end

    elseif u.name == "local" then
      local names = get_names(scope)
      for _, variable in ipairs(u[1]) do
        local def = {
          name = variable;
          type = "variable";
        }
        names[#names + 1] = def
        variable.id = make_identifier()
        if scope.type == "chunk" then
          variable.global = true
        end
        scope[variable.value] = def
      end
      if scope.type == "chunk" then
        u.global = true
      end

    elseif u.name == "return" then
      local results = get_results(scope)
      -- 個数だけ入れておく
      results[#results + 1] = #u[1]

    elseif u.name == "Name" then
      if not u.not_ref or u.def then
        local def, type = assert(find_name(scope, u.value))
        u.id = def.name.id
        u.def_data = def
        if type == "chunk" or type == "external" then
          u.global = true
        end
      end

    elseif u.name == "String" then
      local n = #strings
      strings[n + 1] = u
      u.address = string_offset + n * 8
    end

    for _, v in ipairs(u) do
      visit(v, scope, loop)
    end
  end

  visit(chunk, external_scope)

  local function visit(u)
    local no_recursion

    if u.name == "break" then
      io.write("(br ", u.loop.block_id, ")\n")

    elseif u.name == "while" then
      no_recursion = 1
      io.write("block ", u.block_id, "\n")
      io.write("loop ", u.loop_id, "\n")

    elseif u.name == "if" then
      no_recursion = 1

    elseif u.name == "function" then
      io.write("(func ", u[1].id)
      for _, parameter in ipairs(u[2]) do
        io.write("(param ", parameter.id, " i32)")
      end

      local result
      for _, v in ipairs(u.scope.results) do
        if not result then
          result = v
        else
          assert(result == v)
        end
      end
      if not result then
        result = 0
      end
      u[1].result = result
      if result > 0 then
        io.write "(result"
        for i = 1, result do
          io.write " i32"
        end
        io.write ")"
      end
      io.write "\n"

      for _, name in ipairs(u.scope.names) do
        if name.type == "variable" then
          io.write("(local ", name.name.id, " i32)\n")
        end
      end

    elseif u.name == "local" then
      if u.global then
        for i, name in ipairs(u[1]) do
          io.write("(global ", name.id, " (mut i32)")
          if u[2] and u[2][i] then
            local v = u[2][i]
            assert(v.name == "Integer" or v.name == "String")
            if v.name == "Integer" then
              io.write(" (i32.const ", v.value, ")")
            elseif v.name == "String" then
              io.write(" (i32.const ", v.address, ")")
            end
          end
          io.write ")\n"
        end
        no_recursion = 0
      end

    elseif u.name == "Name" then
      if not u.not_ref then
        assert(u.def_data)
        if u.def_data.type == "function" then
          io.write("(i32.const ", u.def_data.name.index, ")\n")
        else
          if u.global then
            io.write("(global.get ", u.id, ")\n")
          else
            io.write("(local.get ", u.id, ")\n")
          end
        end
      end

    elseif u.name == "-" then
      if u.type == "prefix" then
        io.write "(i32.const 0)\n"
      end

    elseif u.name == "true" then
      io.write "(i32.const 1)\n"

    elseif u.name == "false" then
      io.write "(i32.const 0)\n"

    elseif u.name == "Integer" then
      io.write("(i32.const ", u.value, ")\n")

    elseif u.name == "String" then
      io.write("(i32.const ", u.address, ")\n")
    end

    if no_recursion then
      local n = no_recursion
      if n > #u then
        n = #u
      end
      for i = 1, n do
        visit(u[1], scope)
      end
    else
      for _, v in ipairs(u) do
        visit(v, scope)
      end
    end

    if u.name == "assign" then
      -- 値の代入
      for i = #u[1], 1, -1 do
        local v = u[1][i]
        if v.global then
          io.write("(global.set ", v.id, ")\n")
        else
          io.write("(local.set ", v.id, ")\n")
        end
      end

    elseif u.name == "while" then
      io.write "(i32.eqz)\n"
      io.write("(br_if ", u.block_id, ")\n")

      visit(u[2])

      io.write("(br ", u.loop_id, ")\n")
      io.write "end\n"
      io.write "end\n"

    elseif u.name == "if" then
      -- blockがreturnを含む場合resultが変わるのに対応する必要がある
      io.write "if\n"
      visit(u[2])
      if u[3] then
        io.write "else\n"
        visit(u[3])
      end
      io.write "end\n"

    elseif u.name == "call" then
      local instruction = instruction_table[u[1].id]
      if instruction then
        if type(instruction) == "string" then
          io.write(instruction, "\n")
        else
          instruction(u)
        end
      else
        io.write("(call ", u[1].id, ")\n")
        local v = assert(function_table[u[1].id])
        if u.statement then
          for i = 1, v.result do
            io.write("(drop)\n")
          end
        else
          -- 式の場合も処理するべきだが、とりあえずは手動で対応する
        end
      end

    elseif u.name == "function" then
      io.write ")\n"
      if u[1].value == "main" then
        io.write('(export "_start" (func ', u[1].id, "))\n")
      end

    elseif u.name == "local" then
      if not u.global then
        -- 値の代入
        for i = #u[1], 1, -1 do
          local v = u[1][i]
          io.write("(local.set ", v.id, ")\n")
        end
      end

    elseif u.name == "return" then
      -- returnを直接呼ぶのではなく、末尾にbrして後片付けをするのも良いかも
      io.write "(return)\n"

    elseif u.name == "+" then
      io.write "(i32.add)\n"

    elseif u.name == "-" then
      io.write "(i32.sub)\n"

    elseif u.name == "*" then
      io.write "(i32.mul)\n"

    elseif u.name == "/" then
      io.write "(i32.div_s)\n"

    elseif u.name == "%" then
      io.write "(i32.rem_s)\n"

    elseif u.name == "==" then
      io.write "(i32.eq)\n"

    elseif u.name == "~=" then
      io.write "(i32.ne)\n"

    elseif u.name == "<" then
      io.write "(i32.lt_s)\n"

    end
  end

  local string_address = string_offset + #strings * 8
  for i, string in ipairs(strings) do
    string.data_address = string_address
    string.data_size = make_alignment(#string.value + 1, 8)
    string_address = string_address + string.data_size
  end
  local stack_offset = make_alignment(string_address, 1024)
  local stack_size = 16 * 1024
  local heap_offset = stack_offset + stack_size
  assert(heap_offset < 64 * 1024)

  io.write(([[
(module
(import "wasi_unstable" "fd_write" (func %s (param i32 i32 i32 i32) (result i32)))
(memory 1)
(export "memory" (memory 0))
]]):format(external_scope.fd_write.name.id))

  local function_ids = {}
  for _, v in ipairs(external_scope.names) do
    if v.type == "function" then
      local n = #function_ids + 1
      function_ids[n] = v.name.id
      v.name.index = n
    end
  end
  for _, v in ipairs(chunk.scope.names) do
    if v.type == "function" then
      local n = #function_ids + 1
      function_ids[n] = v.name.id
      v.name.index = n
    end
  end
  io.write("(table ", #function_ids + 1, " funcref)\n")
  io.write "(elem (i32.const 1)"
  for _, id in ipairs(function_ids) do
    io.write(" ", id)
  end
  io.write ")\n"

  local external_data = {
    stack_pointer = stack_offset;
    stack_offset = stack_offset;
    heap_pointer = heap_offset;
    heap_offset = heap_offset;
  }

  for _, v in ipairs(external_scope.names) do
    if v.type == "variable" then
      io.write("(global ", v.name.id, " (mut i32) (i32.const ", external_data[v.name.value], "))\n")
    end
  end

  visit(chunk)

  local function encode_wat_string(s, pattern)
    return (s:gsub(pattern or '[\x00-\x1F\x7F"\\]', function (c)
      return ([[\%02X]]):format(string.byte(c))
    end))
  end

  io.write("(data 0 (i32.const ", string_offset, ') "')
  for i, string in ipairs(strings) do
    io.write(encode_wat_string(("<I4I4"):pack(string.data_address, #string.value), "."))
  end
  for i, string in ipairs(strings) do
    io.write(encode_wat_string(string.value..("\0"):rep(string.data_size - #string.value)))
  end
  io.write '")\n'

  io.write [[
)
]]
end

local source = io.read "*a"
-- dump(io.stdout, tokens):write "\n"
local tokens = lexer(source)
local chunk = parser(tokens)
-- dump(io.stdout, chunk):write "\n"
compiler(chunk)
