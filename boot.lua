-- Copyright (C) 2025 Tomoyuki Fujimori <moyu@dromozoa.com>
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
-- along with dromozoa.  If not, see <https://www.gnu.org/licenses/>.

require "runtime"

--------------------------------------------------------------------------------

local keywords
local symbols1
local symbols2
local symbols3

function lexer_initialize()
  keywords = {
    "and";
    "break";
    "do";
    "else";
    "elseif";
    "end";
    "false";
    "for";
    "function";
    "goto";
    "if";
    "in";
    "local";
    "nil";
    "not";
    "or";
    "repeat";
    "return";
    "then";
    "true";
    "until";
    "while";
  }

  symbols1 = {
    "#";
    "%";
    "&";
    "(";
    ")";
    "*";
    "+";
    ",";
    "-";
    ".";
    "/";
    ":";
    ";";
    "<";
    "=";
    ">";
    "[";
    "]";
    "^";
    "{";
    "|";
    "}";
    "~";
  }

  symbols2 = {
    "..";
    "//";
    "::";
    "<<";
    "<=";
    "==";
    ">=";
    ">>";
    "~=";
  }

  symbols3 = {
    "...";
  }
end

--------------------------------------------------------------------------------

function parse(parser, source_file)
  local source = read_file(source_file)
  local lexer = {
    source_file = source_file;
    source      = source;
    position    = 1;
  }


end

--------------------------------------------------------------------------------

function main()
  lexer_initialize()

  local parser = {}
  local tree = parse(parser, arg[1])
end

export_start(main)
