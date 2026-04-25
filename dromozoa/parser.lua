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

local left_denotation = require "dromozoa.left_denotation"
local node = require "dromozoa.node"
local null_denotation = require "dromozoa.null_denotation"

---@type table<string, dromozoa.null_denotation>
local nud_table = {}

---@type table<string, dromozoa.left_denotation>
local led_table = {}

---@class dromozoa.parser
---@field tokens dromozoa.token[]?
---@field index integer
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.parser",
}

---@return dromozoa.parser
function class.new()
  return setmetatable({
    tokens = nil,
    index = 1,
  }, metatable)
end

---@return dromozoa.token
function class:peek()
  while true do
    local token = self.tokens[self.index]
    if token.kind ~= "Space" and token.kind ~= "Comment" then
      return token
    end
    self.index = self.index + 1
  end
end

---@return dromozoa.token
function class:read()
  local token = self:peek()
  self.index = self.index + 1
  return token
end

---@param token dromozoa.token
---@return dromozoa.node
function class:nud_token(token)
  return node.new(token.kind, token)
end

---@param left dromozoa.node
---@param token dromozoa.token
---@param rbp integer
---@return dromozoa.node
function class:led_left(left, token, rbp)
  return node.new(token.kind, token):append {
    left,
    self:parse_exp(rbp),
  }
end

---@param left dromozoa.node
---@param token dromozoa.token
---@param rbp integer
---@return dromozoa.node
function class:led_right(left, token, rbp)
  return node.new(token.kind, token):append {
    left,
    self:parse_exp(rbp - 1),
  }
end

-- function class:led_call(left, token, rbp)
-- end
--
-- function class:led_subscript(left, token, rbp)
-- end
--
-- function class:led_field(left, token, rbp)
-- end

---@param rbp integer
function class:parse_exp(rbp)
  local token = self:read()
  local nud = nud_table[token.kind]
  if not nud then
    error("parser error at " .. token.srcloc:to_string())
  end
  local left = nud:nud(self, token)
  while true do
    local token = self:peek()
    local led = led_table[token.kind]
    if not led or led.lbp <= rbp then
      break
    end
    self:read()
    left = led:led(self, left, token)
  end
  return left
end

---@param tokens dromozoa.token[]
function class:parse(tokens)
  self.tokens = tokens
  self.index = 1
  return self:parse_exp(0)
end

-- https://www.lua.org/manual/5.5/manual.html#9
do
  nud_table["nil"]     = null_denotation.new(class.nud_token, false)
  nud_table["false"]   = null_denotation.new(class.nud_token, false)
  nud_table["true"]    = null_denotation.new(class.nud_token, false)
  nud_table["Integer"] = null_denotation.new(class.nud_token, false)
end


-- https://www.lua.org/manual/5.5/manual.html#3.4.8
do
  led_table["or"]  = left_denotation.new(100, class.led_left, false)
  led_table["and"] = left_denotation.new(110, class.led_left, false)
  led_table["<"]   = left_denotation.new(120, class.led_left, false)
  led_table[">"]   = left_denotation.new(120, class.led_left, false)
  led_table["<="]  = left_denotation.new(120, class.led_left, false)
  led_table[">="]  = left_denotation.new(120, class.led_left, false)
  led_table["~="]  = left_denotation.new(120, class.led_left, false)
  led_table["=="]  = left_denotation.new(120, class.led_left, false)
  led_table["|"]   = left_denotation.new(130, class.led_left, false)
  led_table["~"]   = left_denotation.new(140, class.led_left, false)
  led_table["&"]   = left_denotation.new(150, class.led_left, false)
  led_table["~"]   = left_denotation.new(160, class.led_left, false)
  led_table["<<"]  = left_denotation.new(170, class.led_left, false)
  led_table[">>"]  = left_denotation.new(170, class.led_left, false)
  led_table[".."]  = left_denotation.new(180, class.led_right, false)
  led_table["+"]   = left_denotation.new(190, class.led_left, false)
  led_table["-"]   = left_denotation.new(190, class.led_left, false)
  led_table["*"]   = left_denotation.new(200, class.led_left, false)
  led_table["/"]   = left_denotation.new(200, class.led_left, false)
  led_table["//"]  = left_denotation.new(200, class.led_left, false)
  led_table["%"]   = left_denotation.new(200, class.led_left, false)
  led_table["^"]   = left_denotation.new(210, class.led_left, false)
  -- led_table["("]   = left_denotation.new(900, class.led_call, true)
  -- led_table["["]   = left_denotation.new(900, class.led_subscript, true)
  -- led_table["."]   = left_denotation.new(900, class.led_field, true)
end

return class
