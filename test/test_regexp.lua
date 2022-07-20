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
--
-- https://github.com/aidansteele/osx-abi-macho-file-format-reference
-- https://developers.wonderpla.net/entry/2021/03/19/105503

--[[

  pattern(n)  .{n}
  pattern(1)  .
  literal     abc
  range       [a-c]
  set         [abc]
  negative    [^abc]
  *
  ?
  action


  range op _"a"-"c"

  R"09"

  S"Xx"




  / char action
  % action

  | "abc" 
  |
  |
  |


  _"abc" "def" "ghi"

  _"a"|"b"|"c"

  _"a".."b"


]]

local metatable = {}

function metatable:__concat(that)
  print("__concat", self, that)
  return setmetatable({}, metatable)
end

function metatable:__shl(that)
  print("__shl", self, that)
  return setmetatable({}, metatable)
end

function metatable:__shr(that)
  print("__shr", self, that)
  return setmetatable({}, metatable)
end

function metatable:__call(that)
  print("__call", self, that)
  return setmetatable({}, metatable)
end

function metatable:__index(that)
  print("__index", self, that)
  return setmetatable({}, metatable)
end

local _ = setmetatable({}, metatable)

local x = _"abc"
local x = _"1".."2".."3"
local x = "4".._"5".."6"
local x = "7".."8".._"9"

local x = _.xyz
local x = _["abc"]
local x = _{"abc"}
local x = _{1,4}
local x = _<<2>>_

--[====[
print(~0)

lexer {
  _"foo";

  LiteralString
    = _"\"" / "clear(fb)" + (quoted_char | _"'" / "append(fb)"){"*"} + _"\""
    ;

  (_" \t" | _"\r" + _"\n"{"?"} | _"\n" + _"\r"{"?"}){"+"} %"skip_token"

  IntegerConstant
    = _["09"] + _["09"]{"*"}
    | _"0" + _{"Xx"} + _["09AFaf"]^"?"
}

local _ = pattern
_() -- any
_(s) -- literal
_[""] -- range
_{""} -- set
_{[[0-9Xx\-\\]]}
]====]

