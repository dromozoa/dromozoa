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

local node = require "dromozoa.node"

---@alias dromozoa.nud fun(parser: dromozoa.parser, token: dromozoa.token): dromozoa.node
---@alias dromozoa.led_function fun(parser: dromozoa.parser, left: dromozoa.node, token: dromozoa.token, rbp: integer): dromozoa.node
---@alias dromozoa.led { lbp: integer, fn: dromozoa.led_function }

---@type table<string, dromozoa.nud>
local exp_nuds
---@type table<string, dromozoa.led>
local exp_leds

---@type table<string, dromozoa.nud>
local prefixexp_nuds
---@type table<string, dromozoa.led>
local prefixexp_leds

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

function class:nud_function(token)
  print(token)
end

function class:nud_name(token)
  print(token)
end

function class:nud_group(token)
  print(token)
end

function class:nud_table(token)
  print(token)
end

function class:nud_prefix(token)
  print(token)
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

function class:led_subscript(left, token, rbp)
  print(left, token, rbp)
end

function class:led_field(left, token, rbp)
  print(left, token, rbp)
end

function class:led_call(left, token, rbp)
  print(left, token, rbp)
end

function class:led_self(left, token, rbp)
  print(left, token, rbp)
end

---@param rbp integer
---@param nuds table<string, dromozoa.nud>
---@param leds table<string, dromozoa.led>
function class:parse_exp_impl(rbp, nuds, leds)
  local token = self:read()
  local nud = nuds[token.kind]
  if not nud then
    error("parser error at " .. token.srcloc:to_string())
  end
  local left = nud(self, token)
  while true do
    local token = self:peek()
    local led = leds[token.kind]
    if not led or led.lbp <= rbp then
      break
    end
    self:read()
    left = led.fn(self, left, token, led.lbp)
  end
  return left
end

---@param rbp integer
function class:parse_prefixexp(rbp)
  return self:parse_exp_impl(rbp, prefixexp_nuds, prefixexp_leds)
end

---@param rbp integer
function class:parse_exp(rbp)
  return self:parse_exp_impl(rbp, exp_nuds, exp_leds)
end

---@param tokens dromozoa.token[]
function class:parse(tokens)
  self.tokens = tokens
  self.index = 1
  return self:parse_exp(0)
end

exp_nuds = {
  ["nil"]      = class.nud_token,
  ["false"]    = class.nud_token,
  ["true"]     = class.nud_token,
  ["Integer"]  = class.nud_token,
  ["String"]   = class.nud_token,
  ["..."]      = class.nud_token,
  ["function"] = class.nud_function,
  ["{"]        = class.nud_table,
  ["-"]        = class.nud_prefix,
  ["not"]      = class.nud_prefix,
  ["#"]        = class.nud_prefix,
  ["~"]        = class.nud_prefix,
}

exp_leds = {
  ["or"]  = { lbp = 100, fn = class.led_left },
  ["and"] = { lbp = 110, fn = class.led_left },
  ["<"]   = { lbp = 120, fn = class.led_left },
  [">"]   = { lbp = 120, fn = class.led_left },
  ["<="]  = { lbp = 120, fn = class.led_left },
  [">="]  = { lbp = 120, fn = class.led_left },
  ["~="]  = { lbp = 120, fn = class.led_left },
  ["=="]  = { lbp = 120, fn = class.led_left },
  ["|"]   = { lbp = 130, fn = class.led_left },
  ["~"]   = { lbp = 140, fn = class.led_left },
  ["&"]   = { lbp = 150, fn = class.led_left },
  ["<<"]  = { lbp = 160, fn = class.led_left },
  [">>"]  = { lbp = 160, fn = class.led_left },
  [".."]  = { lbp = 170, fn = class.led_right },
  ["+"]   = { lbp = 180, fn = class.led_left },
  ["-"]   = { lbp = 180, fn = class.led_left },
  ["*"]   = { lbp = 190, fn = class.led_left },
  ["/"]   = { lbp = 190, fn = class.led_left },
  ["//"]  = { lbp = 190, fn = class.led_left },
  ["%"]   = { lbp = 190, fn = class.led_left },
  ["^"]   = { lbp = 200, fn = class.led_right },
}

prefixexp_nuds = {
  ["Name"] = class.nud_name,
  ["("]    = class.nud_group,
}

prefixexp_leds = {
  ["["]      = { lbp = 900, fn = class.led_subscript },
  ["."]      = { lbp = 900, fn = class.led_field },
  ["("]      = { lbp = 900, fn = class.led_call },
  ["{"]      = { lbp = 900, fn = class.led_call },
  ["String"] = { lbp = 900, fn = class.led_call },
  [":"]      = { lbp = 900, fn = class.led_self },
}

return class
