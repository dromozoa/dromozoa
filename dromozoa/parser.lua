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
local exp_nud_table
---@type table<string, dromozoa.led>
local exp_led_table
---@type integer
local prefix_lbp

---@type table<string, dromozoa.nud>
local prefixexp_nud_table
---@type table<string, dromozoa.led>
local prefixexp_led_table

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
  for i = self.index, #self.tokens do
    local token = self.tokens[i]
    if token.kind ~= "Space" and token.kind ~= "Comment" then
      self.index = i
      return token
    end
  end
  self.index = #self.tokens
  return self.tokens[self.index]
end

---@return dromozoa.token
function class:read()
  local token = self:peek()
  self.index = self.index + 1
  return token
end

function class:unread()
  self.index = self.index - 1
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
  return node.new(token.kind, token):append {
    self:parse_exp(prefix_lbp)
  }
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
---@param nud_table table<string, dromozoa.nud>
---@param led_table table<string, dromozoa.led>
---@return dromozoa.node?
---@return string?
function class:parse_exp_impl(rbp, nud_table, led_table)
  local token = self:read()
  local nud = nud_table[token.kind]
  if not nud then
    self:unread()
    return nil, "parser error at " .. token.srcloc:to_string()
  end
  local result = nud(self, token)
  while true do
    local token = self:peek()
    local led = led_table[token.kind]
    if not led or led.lbp <= rbp then
      break
    end
    self:read()
    result = led.fn(self, result, token, led.lbp)
  end
  return result
end

---@param rbp integer
---@return dromozoa.node?
---@return string?
function class:parse_exp(rbp)
  local result = self:parse_prefixexp(rbp)
  if result then
    return result
  end
  return self:parse_exp_impl(rbp, exp_nud_table, exp_led_table)
end

---@param rbp integer
---@return dromozoa.node?
---@return string?
function class:parse_prefixexp(rbp)
  return self:parse_exp_impl(rbp, prefixexp_nud_table, prefixexp_led_table)
end

---@param tokens dromozoa.token[]
function class:parse(tokens)
  self.tokens = tokens
  self.index = 1
  return self:parse_exp(0)
end

exp_nud_table = {
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

exp_led_table = {
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
  ["^"]   = { lbp = 210, fn = class.led_right },
}

-- 前置演算子の優先順位は(* / // %)と^の間
prefix_lbp = 200

prefixexp_nud_table = {
  ["Name"] = class.nud_token,
  ["("]    = class.nud_group,
}

prefixexp_led_table = {
  ["["]      = { lbp = 100, fn = class.led_subscript },
  ["."]      = { lbp = 100, fn = class.led_field },
  ["("]      = { lbp = 100, fn = class.led_call },
  ["{"]      = { lbp = 100, fn = class.led_call },
  ["String"] = { lbp = 100, fn = class.led_call },
  [":"]      = { lbp = 100, fn = class.led_self },
}

return class
