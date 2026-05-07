-- Copyright (C) 2026 Tomoyuki Fujimori <moyu@dromozoa.com>
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

--=========================================================================

---@alias dromozoa.annotation_parser.nud fun(parser: dromozoa.annotation_parser, x: dromozoa.token): dromozoa.node
---@alias dromozoa.annotation_parser.led_function fun(parser: dromozoa.annotation_parser, u: dromozoa.node, x: dromozoa.token, rbp: integer): dromozoa.node
---@alias dromozoa.annotation_parser.led { lbp: integer, fn: dromozoa.annotation_parser.led_function }

---@type table<string, dromozoa.annotation_parser.nud>
local nud_table
---@type table<string, dromozoa.annotation_parser.led>
local led_table
-- ---@type integer
-- local prefix_lbp

--=========================================================================

---@class dromozoa.annotation_parser
---@field lexer dromozoa.annotation_lexer
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.annotation_parser",
}

---@param lexer dromozoa.annotation_lexer
---@return dromozoa.annotation_parser
function class.new(lexer)
  return setmetatable({
    lexer = lexer,
  }, metatable)
end

---@return dromozoa.token
function class:peek()
  return self.lexer:peek()
end

---@return dromozoa.token
function class:read()
  return self.lexer:read()
end

function class:unread()
  self.lexer:unread()
end

--=========================================================================

function class:nud_token(x)
  return x:new_expression_node()
end

--=========================================================================

function class:led_left(u, x, rbp)
  return x:new_expression_node():extend { u, self:parse_expression(rbp) }
end

function class:led_suffix(u, x)
  return x:new_expression_node():append(u)
end

--=========================================================================

function class:parse_expression(rbp)
  local x = self:read()
  local nud = nud_table[x.kind]
  if not nud then
    self:unread()
    error("syntax error at " .. x.srcloc:to_string())
  end
  local u = nud(self, x)
  while true do
    local x = self:read()
    local led = led_table[x.kind]
    if not led or led.lbp <= rbp then
      self:unread()
      break
    end
    u = led.fn(self, u, x, led.lbp)
  end
  return u
end

--=========================================================================

nud_table = {
  ["Name"] = class.nud_token,
}

led_table = {
  ["|"] = { lbp = 100, fn = class.led_left },
  ["?"] = { lbp = 110, fn = class.led_suffix },
}

return class
