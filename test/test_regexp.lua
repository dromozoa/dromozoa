-- Copyright (C) 2020-2022 Tomoyuki Fujimori <moyu@dromozoa.com>
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

local write_graphviz = require "dromozoa.regexp.write_graphviz"

local array = require "dromozoa.array"
local pattern = require "dromozoa.regexp.pattern"
local machine = require "dromozoa.regexp.machine"
local compile = require "dromozoa.regexp.compile"

local _ = pattern
local union = machine.union
local guard = machine.guard
local lexer = machine.lexer

local m1 = union {
  _{ _"a"*"*" + _"b"*"+" + _"c"/"print'T'"*"?" - "abc" ; _["xyz"]*{3} } %"print'A'"
}

local out = assert(io.open("test-m1.dot", "w"))
write_graphviz(out, m1.start_state)
out:close()

local token_names = array()

local m2 = machine.union {
  -- _;
  _"aaa" %"print'a'";
  _"aba" %"print'b'";
  _{"ab"}*{3} %"print'b'";
}

local out = assert(io.open("test-m2.dot", "w"))
write_graphviz(out, m2.start_state)
out:close()

local m3 = machine.lexer(token_names, {
  _"if";
  _"then";
  _"else";
  _"elseif";
  _"end";
  integer = (_["09"]/"print'i'")*1;
  string = _"\"" + (-_["\""]/"print'c'")*0 + "\"";
  _{" \t\r\n"}*1;
  -- line_comment = ("--" + -_{"\n\r"}*0) - ("--[[" + _*0);
  line_comment = "--" + (-_{"\n\r"}*0 - ("[[" + _*0));
  block_comment = "--[[" + (_*0 - (_*0 + "]]" + _*0) - (_*0 + "]")) + "]]";
  c_comment = "/*" + (_*0 - (_*0 + "*/" + _*0)) + "*/";
  -- any = _;
})

local out = assert(io.open("test-m3.dot", "w"))
write_graphviz(out, m3.start_state)
out:close()

local r = compile {
  m1 = m1;
  m2 = m2;
  m3;
}
local out = assert(io.open("test-gen.lua", "w"))
out:write(r)
out:close()
