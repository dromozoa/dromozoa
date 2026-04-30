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

--=========================================================================

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
    if not token:check("Space", "Comment") then
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

---@return dromozoa.token
function class:unread()
  for i = self.index - 1, 1, -1 do
    local token = self.tokens[i]
    if not token:check("Space", "Comment") then
      self.index = i
      return token
    end
  end
  self.index = 1
  return self.tokens[self.index]
end

--=========================================================================

---@param token dromozoa.token
---@return dromozoa.node
function class:nud_token(token)
  return token:to_node()
end

---@diagnostic disable-next-line: unused-local
function class:nud_function(token)
  error "not implemented"
end

---@param token dromozoa.token
---@return dromozoa.node
function class:nud_table(token)
  return self:parse_table(token)
end

---@param token dromozoa.token
---@return dromozoa.node
function class:nud_prefix(token)
  return token:to_node():append(self:parse_exp(prefix_lbp))
end

---@param token dromozoa.token
---@return dromozoa.node
function class:nud_group(token)
  local result = node.new("group", token):append(self:parse_exp(0))
  self:read():require ")"
  return result
end

--=========================================================================

---@param left dromozoa.node
---@param token dromozoa.token
---@param rbp integer
---@return dromozoa.node
function class:led_left(left, token, rbp)
  return token:to_node():extend {
    left,
    self:parse_exp(rbp),
  }
end

---@param left dromozoa.node
---@param token dromozoa.token
---@param rbp integer
---@return dromozoa.node
function class:led_right(left, token, rbp)
  return token:to_node():extend {
    left,
    self:parse_exp(rbp - 1),
  }
end

---@param left dromozoa.node
---@param token dromozoa.token
---@param rbp integer
---@return dromozoa.node
---@diagnostic disable-next-line: unused-local
function class:led_index(left, token, rbp)
  local result = node.new("index", token):extend {
    left,
    self:parse_exp(0),
  }
  self:read():require "]"
  return result
end

---@param left dromozoa.node
---@param token dromozoa.token
---@param rbp integer
---@return dromozoa.node
---@diagnostic disable-next-line: unused-local
function class:led_property(left, token, rbp)
  return node.new("property", token):extend {
    left,
    self:read():require "Name":to_node(),
  }
end

---@param left dromozoa.node
---@param token dromozoa.token
---@param rbp integer
---@return dromozoa.node
---@diagnostic disable-next-line: unused-local
function class:led_call(left, token, rbp)
  return node.new("call", token):extend {
    left,
    self:parse_args(token),
  }
end

---@param left dromozoa.node
---@param token dromozoa.token
---@param rbp integer
---@return dromozoa.node
---@diagnostic disable-next-line: unused-local
function class:led_self(left, token, rbp)
  return node.new("self", token):extend {
    left,
    self:read():require "Name":to_node(),
    self:parse_args(self:read()),
  }
end

--=========================================================================

---@param nud_table table<string, dromozoa.nud>
---@return dromozoa.node?
---@return string?
function class:parse_nud(nud_table)
  local token = self:read()
  local nud = nud_table[token.kind]
  if not nud then
    self:unread()
    return nil, "syntax error at " .. token.srcloc:to_string()
  end
  return nud(self, token)
end

---@param left dromozoa.node
---@param rbp integer
---@param led_table table<string, dromozoa.led>
---@return dromozoa.node
function class:parse_led(left, rbp, led_table)
  while true do
    local token = self:read()
    local led = led_table[token.kind]
    if not led or led.lbp <= rbp then
      self:unread()
      break
    end
    left = led.fn(self, left, token, led.lbp)
  end
  return left
end

--=========================================================================

function class:parse_stat()
  local token = self:read()
  if token:check ";" or token:check "break" then
    return token:to_node()
  elseif token:check "::" then
    local result = node.new("label", token):append(self:read():require "Name":to_node())
    self:read():require "::"
    return result
  elseif token:check "goto" then
    return token:to_node():append(self:read():require "Name":to_node())
  else
    local prefixexp = self:parse_prefixexp(0)
    -- if prefixexp:check("call", "self") then
    --   return prefixexp
    -- end

  end
end

---@param rbp integer
---@return dromozoa.node
function class:parse_exp(rbp)
  local left = self:parse_nud(prefixexp_nud_table)
  if left then
    left = self:parse_led(left, 0, prefixexp_led_table)
  else
    left = assert(self:parse_nud(exp_nud_table))
  end
  return self:parse_led(left, rbp, exp_led_table)
end

---@param rbp integer
---@return dromozoa.node
function class:parse_prefixexp(rbp)
  local left = assert(self:parse_nud(prefixexp_nud_table))
  return self:parse_led(left, rbp, prefixexp_led_table)
end

---@param token dromozoa.token
---@return dromozoa.node
function class:parse_args(token)
  if token:check "(" then
    local result = node.new("args", token)
    if self:peek():check ")" then
      self:read()
      return result
    end
    result:append(self:parse_exp(0))
    while true do
      local token = self:read()
      if token:check ")" then
        return result
      end
      token:require ","
      result:append(self:parse_exp(0))
    end
  elseif token:check "{" then
    return node.new "args":append(self:parse_table(token))
  else
    token:require "String"
    return node.new "args":append(token:to_node())
  end
end

---@param token dromozoa.token
---@return dromozoa.node
function class:parse_table(token)
  local result = node.new("table", token)
  while true do
    if self:peek():check "}" then
      self:read()
      return result
    end
    result:append(self:parse_field())
    local token = self:read()
    if token:check "}" then
      return result
    end
    token:require(",", ";")
  end
end

---@return dromozoa.node
function class:parse_field()
  local token = self:read()
  if token:check "[" then
    local index = self:parse_exp(0)
    self:read():require "]"
    self:read():require "="
    return node.new("index_field", token):extend {
      index,
      self:parse_exp(0),
    }
  elseif token:check "Name" and self:peek():check "=" then
    local index = token:to_node()
    self:read()
    return node.new("property_field", token):extend {
      index,
      self:parse_exp(0),
    }
  else
    self:unread()
    return node.new "list_field":append(self:parse_exp(0))
  end
end

--=========================================================================

---@param tokens dromozoa.token[]
function class:parse(tokens)
  self.tokens = tokens
  self.index = 1
  return self:parse_exp(0)
end

--=========================================================================

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
  ["["]      = { lbp = 900, fn = class.led_index },
  ["."]      = { lbp = 900, fn = class.led_property },
  ["("]      = { lbp = 900, fn = class.led_call },
  ["{"]      = { lbp = 900, fn = class.led_call },
  ["String"] = { lbp = 900, fn = class.led_call },
  [":"]      = { lbp = 900, fn = class.led_self },
}

return class
