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

--[====[
  LuaのパターンをJavaScriptの正規表現に変換する

  character classのpatternは使わないものとする
  %zだけは使える

  character classは自分でなんとかする

  -- %zは\u0000で置き換える
  -- それ以外の%Xは\Xで置き換える
  -- %-でなく、character classでない-を*?で置き換える

  ..- => .+?
  .- => .*?

build_runtime.lua:  :gsub("%-%-%[(%=*)%[.-%]%1%]", "")
build_runtime.lua:  :gsub("%-%-[^\n]*", "")
build_runtime.lua:  :gsub("[ \t]+\n", "\n")
build_runtime.lua:  :gsub("\n\n+", "\n")
build_runtime.lua:  :gsub("^\n+", "")
build_runtime.lua:  :gsub("(.-)%$([A-Za-z_][0-9A-Za-z_]*)", function (a, b)
compile_stage1.lua:      local chunk = parse(name:gsub("%.", "/") .. ".lua")
compile_stage1.lua:local source_map_basename = source_map_filename:gsub(".*%/", "")
dromozoa//regexp/compile.lua:    :gsub("%$([A-Za-z_][0-9A-Za-z_]*)", context.action.variables)
dromozoa//regexp/compile.lua:    :gsub("%$%{%'(..-)%'%}", context.action.variables)
dromozoa//regexp/compile.lua:    :gsub("%$%{%<(..-)%>%}", function (a)
dromozoa//regexp/compile.lua:    :gsub("(.-[^0-9A-Za-z_]fcall[\t-\r ]*%(.-%))[\t-\r ]*", function (a)
dromozoa//regexp/compile.lua:      append(actions, insert(context.action, "function()" .. a:gsub("^[\t-\r ]+", "") .. "\nend;\n"))
dromozoa//regexp/compile.lua:  append(actions, insert(context.action, "function()" .. s:gsub("^[\t-\r ]+", "") .. "\nend;\n"))
dromozoa//quote_lua.lua:  return '"' .. s:gsub("[%z\1-\31\"\\\127]", quote) .. '"'
dromozoa//parser/compile.lua:      :gsub("%$([A-Za-z_][0-9A-Za-z_]*)", grammar.symbol_table)
dromozoa//parser/compile.lua:      :gsub("%$%{%'(..-)%'%}", grammar.symbol_table)
dromozoa//parser/compile.lua:      :gsub("%$([1-9][0-9]*)", "S[%1]")
dromozoa//parser/compile.lua:      :gsub("%$0", "S[0]")
dromozoa//parser/compile.lua:      :gsub("%$%$", "SS")
dromozoa//quote_js.lua:  return '"' .. s:gsub("[%z\1-\31\"\\\127]", quote):gsub(LS, [[\u2028]]):gsub(PS, [[\u2029]]) .. '"'
dromozoa//compiler/stage1.lua:]]):gsub("\n", {})

]====]
