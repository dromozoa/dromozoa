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

local left_denotion = require "dromozoa.left_denotion"
local node = require "dromozoa.node"
local null_denotion = require "dromozoa.null_denotion"

---@type table<string, dromozoa.null_denotion>
local nud_table = {}

---@type table<string, dromozoa.left_denotion>
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
---@param min_bp integer
---@return dromozoa.node
function class:led_left(left, token, min_bp)
  return node.new(token.kind, token):append {
    left,
    self:parse_exp(min_bp),
  }
end

---@param left dromozoa.node
---@param token dromozoa.token
---@param min_bp integer
---@return dromozoa.node
function class:led_right(left, token, min_bp)
  return node.new(token.kind, token):append {
    left,
    self:parse_exp(min_bp - 1),
  }
end

---@param min_bp integer
function class:parse_exp(min_bp)
  local token = self:read()
  local nud = nud_table[token.kind]
  if not nud then
    error("parser error at " .. token.srcloc:to_string())
  end
  local result = nud.denotion(self, token)
  while true do
    local token = self:peek()
    local led = led_table[token.kind]
    if not led or led.bp <= min_bp then
      break
    end
    self:read()
    result = led.denotion(self, result, token, led.bp)
  end
  return result
end

---@param tokens dromozoa.token[]
function class:parse(tokens)
  self.tokens = tokens
  self.index = 1
  return self:parse_exp(0)
end

do
  nud_table["false"]   = null_denotion.new(class.nud_token, false)
  nud_table["nil"]     = null_denotion.new(class.nud_token, false)
  nud_table["true"]    = null_denotion.new(class.nud_token, false)
  nud_table["Integer"] = null_denotion.new(class.nud_token, false)
end

do
  local bp = 10
  led_table["+"] = left_denotion.new(bp, class.led_left, false)
  led_table["-"] = left_denotion.new(bp, class.led_left, false)
end

return class
