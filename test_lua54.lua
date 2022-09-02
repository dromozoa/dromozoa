-- Copyright (C) 2022 Tomoyuki Fujimori <moyu@dromozoa.com>
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
-- along with dromozoa.  If not, see <http://www.gnu.org/licenses/>.

local verbose = os.getenv "VERBOSE" == "1"

local dir = assert(...)

local lua54_regexp = require "dromozoa.compiler.lua54_regexp"
local lua54_parser = require "dromozoa.compiler.lua54_parser"
local lua54_generator = require "dromozoa.compiler.lua54_generator"

---------------------------------------------------------------------------

local quotes = {}
for byte = 0x00, 0x1F do
  quotes[string.char(byte)] = ([[\x%02X]]):format(byte)
end
quotes["\b"] = [[\b]]
quotes["\t"] = [[\t]]
quotes["\n"] = [[\n]]
quotes["\v"] = [[\v]]
quotes["\f"] = [[\f]]
quotes["\r"] = [[\r]]

local LS = string.char(0xE2, 0x80, 0xA8)
local PS = string.char(0xE2, 0x80, 0xA9)

local function quote(s)
  return '"' .. s:gsub("[\0-\31\"\\]", quotes):gsub(LS, [[\u2028]]):gsub(PS, [[\u2029]]) .. '"'
end

local function generate_proto_code(out, protos, u, n)
  n = n + 1

  local u_name = u[0]
  local a = u.a
  local b = u.b

  out:write(("  "):rep(n))
  if u_name == "break" then
    out:write "break;"

  elseif u_name == "if" then
    out:write "a=S.pop();"
    out:write "if (a!==undefined&&a!==false) {\n"
    for _, v in ipairs(u[1]) do
      generate_proto_code(out, protos, v, n)
    end
    out:write(("  "):rep(n), "} else {\n")
    for _, v in ipairs(u[2]) do
      generate_proto_code(out, protos, v, n)
    end
    out:write(("  "):rep(n), "}")

  elseif u_name == "loop" then
    out:write "do {\n"
    for _, v in ipairs(u) do
      generate_proto_code(out, protos, v, n)
    end
    out:write(("  "):rep(n), "} while (true);")

  elseif u_name == "add" then out:write "b=S.pop();a=S.pop();S.push(a+b);"
  elseif u_name == "sub" then out:write "b=S.pop();a=S.pop();S.push(a-b);"
  elseif u_name == "mul" then out:write "b=S.pop();a=S.pop();S.push(a*b);"
  elseif u_name == "div" then out:write "b=S.pop();a=S.pop();S.push(a/b);"
  elseif u_name == "idiv" then out:write "b=S.pop();a=S.pop();S.push(Math.floor(a/b));"
  elseif u_name == "mod" then out:write "b=S.pop();a=S.pop();S.push((a%b+b)%b);"
  elseif u_name == "pow" then out:write "b=S.pop();a=S.pop();S.push(Math.pow(a,b));"
  elseif u_name == "band" then out:write "b=S.pop();a=S.pop();S.push(a&b);"
  elseif u_name == "bxor" then out:write "b=S.pop();a=S.pop();S.push(a^b);"
  elseif u_name == "bor" then out:write "b=S.pop();a=S.pop();S.push(a|b);"
  elseif u_name == "shr" then out:write "b=S.pop();a=S.pop();S.push(a>>b);"
  elseif u_name == "shl" then out:write "b=S.pop();a=S.pop();S.push(a<<b);"
  elseif u_name == "concat" then out:write "b=S.pop();a=S.pop();S.push(a.toString()+b.toString());"
  elseif u_name == "lt" then out:write "b=S.pop();a=S.pop();S.push(a<b);"
  elseif u_name == "le" then out:write "b=S.pop();a=S.pop();S.push(a<=b);"
  elseif u_name == "gt" then out:write "b=S.pop();a=S.pop();S.push(a>b);"
  elseif u_name == "ge" then out:write "b=S.pop();a=S.pop();S.push(a>=b);"
  elseif u_name == "eq" then out:write "b=S.pop();a=S.pop();S.push(a===b);"
  elseif u_name == "ne" then out:write "b=S.pop();a=S.pop();S.push(a!==b);"

  elseif u_name == "unm" then out:write "a=S.pop();S.push(-a);"
  elseif u_name == "not" then out:write "a=S.pop();S.push(a===undefined||a===false);"
  elseif u_name == "len" then out:write "b=1;a=S.pop();for(;a.get(b)!==undefined;++b);S.push(b-1);"
  elseif u_name == "bnot" then out:write "a=S.pop();S.push(~a);"

  elseif u_name == "new_local" then
    out:write("V", a, "=[S.pop()];")

  elseif u_name == "tbc_local" then
    -- TODO tbcを実装する
    out:write("V", a, "=[S.pop()];")

  elseif u_name == "set_local" then
    out:write("V", a, "[0]=S.pop();")

  elseif u_name == "set_upvalue" then
    out:write("U", a, "[0]=S.pop();")

  elseif u_name == "set_field" then
    out:write "c=S.pop();"
    out:write("b=S[", b - 1, "];")
    out:write("a=S[", a - 1, "];")
    out:write "a.set(b, c);"

  elseif u_name == "set_table" then
    out:write "c=S.pop();"
    out:write "b=S.pop();"
    out:write("a=S[", a - 1, "];")
    out:write "a.set(b, c);"

  elseif u_name == "get_local" then
    out:write("S.push(V", a, "[0]);")

  elseif u_name == "get_upvalue" then
    out:write("S.push(U", a, "[0]);")

  elseif u_name == "get_table" then
    out:write "b=S.pop();"
    out:write "a=S.pop();"
    out:write "S.push(a.get(b));"

  elseif u_name == "new_table" then
    out:write "S.push(new Map());"

  elseif u_name == "closure" then
    out:write("S.push(P", a, "(")
    for i, v in ipairs(protos[a].upvalues) do
      if i > 1 then
        out:write ", "
      end
      if v.var < 0 then
        out:write("U", -v.var)
      else
        out:write("V", v.var)
      end
    end
    out:write "));"

  elseif u_name == "push_false" then
    out:write "S.push(false);"

  elseif u_name == "push_true" then
    out:write "S.push(true);"

  elseif u_name == "push_literal" then
    out:write("S.push(", quote(a), ");")

  elseif u_name == "push_numeral" then
    -- TODO hexadecimal floatをどうにかする
    out:write("S.push(", a, ");")

  elseif u_name == "dup" then
    out:write "S.push(S[S.length-1]);"

  elseif u_name == "return" then
    out:write "return S"

  elseif u_name == "call" then
    out:write("b=S.splice(", a, ");")
    out:write "a=S.pop();"
    if b == 0 then
      out:write "a(...b);"
    else
      out:write "c=a(...b);"
      if b ~= -1 then
        out:write("if (c.length<", b, ") c[", b - 1, "]=undefined; else c=c.slice(0,", b, ");")
      end
      out:write "S.push(...c);"
    end

  elseif u_name == "self" then
    out:write("c=S.splice(", a + 1, ");")
    out:write "b=S.pop();"
    out:write "a=S.pop();"

    if b == 0 then
      out:write "(a.get(b))(a, ...c);"
    else
      out:write "c=(a.get(b))(...c);"
      if b ~= -1 then
        out:write("if (c.length<", b, ") c[", b - 1, "]=undefined; else c=c.slice(0,", b, ");")
      end
      out:write "S.push(...c);"
    end


  elseif u_name == "set_list" then
    out:write("b=S.splice(", a, ");")
    out:write("a=S[", a - 1, "];")
    out:write("for (let i=0; i<b.length; ++i) a.set(i+1, b[i]);")

  elseif u_name == "push_nil" then
    out:write "S.push("
    for i = 1, a do
      if i > 1 then
        out:write ", "
      end
      out:write "undefined"
    end
    out:write ");"

  elseif u_name == "pop" then
    out:write("S.splice(-", a, ");")

  else
    out:write("/* ", u_name , " */")
  end
  out:write "\n"
end

local function generate_proto(out, protos, proto)
  out:write("const P", proto.index, " = (")
  for i = 1, #proto.upvalues do
    if i > 1 then
      out:write ", "
    end
    out:write("U", i)
  end
  out:write ") => {\n"
  out:write "  return ("
  for i = 1, proto.nparams do
    if i > 1 then
      out:write ", "
    end
    out:write("A", i)
  end
  if proto.vararg then
    if proto.nparams > 0 then
      out:write ", "
    end
    out:write "...VA"
  end
  out:write ") => {\n"

  out:write "    let S=[], a, b, c;\n"
  for i = 1, #proto.locals do
    out:write("    let V", i)
    if i <= proto.nparams then
      out:write("=[A", i, "]")
    end
    out:write ";\n"
  end

  for _, v in ipairs(proto.code) do
    generate_proto_code(out, protos, v, 1)
  end

  out:write "  };\n"
  out:write "};\n"
end

local function generate_chunk(out)
  out:write [[
const fs = require("fs");
const io = new Map();
io.set("write", (s) => {
  fs.writeSync(1, s);
});
const env = new Map();
env.set("io", io);
const chunk = P1([env]);
chunk(...process.argv.slice(2));
]]
end

---------------------------------------------------------------------------

local quotes = {
  ['&'] = '&amp;';
  ['<'] = '&lt;';
  ['>'] = '&gt;';
  ['"'] = '&quot;';
}

local function quote(s)
  return '"' .. string.gsub(s, '[&<>"]', quotes) .. '"'
end

local function dump_attrs(out, u, attrs)
  for _, attr in ipairs(attrs) do
    local v = u[attr]
    if v ~= nil then
      local t = type(v)
      if t == "boolean" or t == "number" or t == "string" then
        out:write(" ", attr, "=", quote(tostring(v)))
      elseif #v > 0 then
        out:write(" ", attr, "=", quote(table.concat(v, ",")))
      end
    end
  end
end

local node_attrs = {
  "v";
  "declare", "resolve", "define", "label";
  "var", "env";
  "adjust", "nr";
  "loop";
  "self", "vararg";
  "attribute";
  "binop", "unop";
  "hint";
  "end_of_scope";
  "stack";
}

local function dump_node(out, u, n)
  if n == nil then
    n = 0
  else
    n = n + 1
  end

  out:write(("  "):rep(n), "<node")
  if u[0] ~= nil then
    out:write(" name=", quote(lua54_parser.symbol_names[u[0]]))
  end
  dump_attrs(out, u, node_attrs)

  if #u == 0 then
    out:write "/>\n"
  else
    out:write ">\n"
    for _, v in ipairs(u) do
      dump_node(out, v, n)
    end
    out:write(("  "):rep(n), "</node>\n")
  end
end

local function dump_proto_list(out, list, list_name, name)
  if #list == 0 then
    out:write("    <", list_name, "/>\n")
  else
    out:write("    <", list_name, ">\n")
    for i, v in ipairs(list) do
      out:write("      <", name, " index=\"", i, "\"")
      dump_attrs(out, v, { "name", "attribute", "var" })
      if v.node then
        dump_attrs(out, v.node, { "n", "c" })
      end
      out:write "/>\n"
    end
    out:write("    </", list_name, ">\n")
  end
end

local function dump_proto_code(out, u, n)
  if n == nil then
    n = 0
  else
    n = n + 1
  end

  out:write(("  "):rep(n), "<code op=", quote(u[0]))
  if u.a ~= nil then
    out:write(" a=", quote(u.a))
  end
  if u.b ~= nil then
    out:write(" b=", quote(u.b))
  end

  if #u == 0 then
    out:write "/>\n"
  else
    out:write ">\n"
    for _, v in ipairs(u) do
      dump_proto_code(out, v, n)
    end
    out:write(("  "):rep(n), "</code>\n")
  end
end

local function dump_protos(out, protos)
  out:write "<protos>\n"
  for _, proto in ipairs(protos) do
    out:write "  <proto"
    dump_attrs(out, proto, { "index", "nparams", "self", "vararg" })
    out:write ">\n"

    dump_proto_list(out, proto.locals, "locals", "local")
    dump_proto_list(out, proto.upvalues, "upvalues", "upvalue")
    dump_proto_list(out, proto.labels, "labels", "label")

    out:write "    <scopes>\n"
    for _, scope in ipairs(proto.scopes) do
      out:write "      <scope"
      dump_attrs(out, scope, { "index", "repeat_until", "labels", "locals" })
      out:write "/>\n"
    end
    out:write "    </scopes>\n"

    if #proto.code == 0 then
      out:write "    <codes/>\n"
    else
      out:write "    <codes>\n"
      for _, v in ipairs(proto.code) do
        dump_proto_code(out, v, 2)
      end
      out:write "    </codes>\n"
    end

    out:write "  </proto>\n"
  end
  out:write "</protos>\n"
end

for i = 2, #arg do
  local source_filename = assert(arg[i])
  local result_basename = assert(source_filename:match "([^/]+)%.lua$")
  result_basename = dir .. "/" .. result_basename

  local handle = assert(io.open(source_filename))
  local source = handle:read "*a"
  handle:close()

  local out = assert(io.open(result_basename .. "_list.xml", "w"))
  out:write "<nodes>\n"

  local parse = lua54_parser()
  local root = lua54_regexp(source, source_filename, lua54_parser.max_terminal_symbol, function (token)
    dump_node(out, token)
    return parse(token)
  end)

  out:write "</nodes>\n"
  out:close()

  local protos = lua54_generator(root)

  local out = assert(io.open(result_basename .. "_tree.xml", "w"))
  dump_node(out, root)
  out:close()

  local out = assert(io.open(result_basename .. "_protos.xml", "w"))
  dump_protos(out, protos)
  out:close()

  local out = assert(io.open(result_basename .. ".js", "w"))
  for i = #protos, 1, -1 do
    generate_proto(out, protos, protos[i])
  end
  generate_chunk(out);
  out:close()
end
