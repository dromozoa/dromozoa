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

--=========================================================================

---@param kind string
---@return dromozoa.node
local function new_block_node(kind)
  return node.new("block", kind)
end

---@param kind string
---@return dromozoa.node
local function new_statement_node(kind)
  return node.new("statement", kind)
end

---@param kind string
---@return dromozoa.node
local function new_auxiliary_node(kind)
  return node.new("auxiliary", kind)
end

--=========================================================================

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
  return token:new_expression_node()
end

function class:nud_function(token)
  return token:new_expression_node "function":append(self:parse_funcbody())
end

---@param token dromozoa.token
---@return dromozoa.node
function class:nud_table(token)
  return self:parse_table(token)
end

---@param token dromozoa.token
---@return dromozoa.node
function class:nud_prefix(token)
  return token:new_expression_node():append(self:parse_exp(prefix_lbp))
end

---@param token dromozoa.token
---@return dromozoa.node
function class:nud_group(token)
  local result = token:new_expression_node "group":append(self:parse_exp())
  self:read():require ")"
  return result
end

--=========================================================================

---@param left dromozoa.node
---@param token dromozoa.token
---@param rbp integer
---@return dromozoa.node
function class:led_left(left, token, rbp)
  return token:new_expression_node():extend {
    left,
    self:parse_exp(rbp),
  }
end

---@param left dromozoa.node
---@param token dromozoa.token
---@param rbp integer
---@return dromozoa.node
function class:led_right(left, token, rbp)
  return token:new_expression_node():extend {
    left,
    self:parse_exp(rbp - 1),
  }
end

---@param u dromozoa.node
---@param x dromozoa.token
---@param rbp integer
---@return dromozoa.node
---@diagnostic disable-next-line: unused-local
function class:led_index(u, x, rbp)
  local v = self:parse_exp()
  self:read():require "]"
  return x:new_expression_node "index":extend { u, v }
end

---@param u dromozoa.node
---@param x dromozoa.token
---@param rbp integer
---@return dromozoa.node
---@diagnostic disable-next-line: unused-local
function class:led_member(u, x, rbp)
  local v = self:read():require "Name":new_expression_node()
  return x:new_expression_node "member":extend { u, v }
end

---@param u dromozoa.node
---@param x dromozoa.token
---@param rbp integer
---@return dromozoa.node
---@diagnostic disable-next-line: unused-local
function class:led_call(u, x, rbp)
  local v = self:parse_args(x)
  return x:new_expression_node "call":extend { u, v }
end

---@param left dromozoa.node
---@param token dromozoa.token
---@param rbp integer
---@return dromozoa.node
---@diagnostic disable-next-line: unused-local
function class:led_self(left, token, rbp)
  return token:new_expression_node "self":extend {
    left,
    self:read():require "Name":new_expression_node(),
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

---@return string ...
local function stat_terminals()
  return "end", "until", "elseif", "else", "EOF"
end

function class:parse_block()
  local result = new_block_node "block"
  while true do
    local token = self:read()
    if token:check "return" then
      return result:append(self:parse_retstat(token))
    elseif token:check(stat_terminals()) then
      self:unread()
      return result
    end

    self:unread()
    result:append(self:parse_stat())
  end
end

function class:parse_stat()
  local token = self:read()
  if token:check ";" or token:check "break" then
    return token:new_statement_node()
  elseif token:check "::" then
    local result = token:new_statement_node "label":append(self:read():require "Name":new_auxiliary_node())
    self:read():require "::"
    return result
  elseif token:check "break" then
    return token:new_statement_node()
  elseif token:check "goto" then
    return token:new_statement_node():append(self:read():require "Name":new_auxiliary_node())
  elseif token:check "do" then
    local result = token:new_statement_node():append(self:parse_block())
    self:read():require "end"
    return result
  elseif token:check "while" then
    local result = token:new_statement_node():append(self:parse_exp())
    self:read():require "do"
    result:append(self:parse_block())
    self:read():require "end"
    return result
  elseif token:check "repeat" then
    local result = token:new_statement_node():append(self:parse_block())
    self:read():require "until"
    result:append(self:parse_exp())
    return result
  elseif token:check "if" then
    local result = self:parse_if(token)
    self:read():require "end"
    return result
  elseif token:check "for" then
    local name = self:read():require "Name"
    if self:peek():check "=" then
      self:read()
      local explist = new_auxiliary_node "explist"
      explist:append(self:parse_exp())
      self:read():require ","
      explist:append(self:parse_exp())
      local token = self:read()
      if token:check "," then
        explist:append(self:parse_exp())
        token = self:read()
      end
      token:require "do"
      local result = token:new_statement_node "numeric_for":extend {
        name:new_expression_node(),
        explist,
        self:parse_block(),
      }
      self:read():require "end"
      return result
    else
      local namelist = new_auxiliary_node "namelist":append(name:new_auxiliary_node())
      while true do
        local token = self:read()
        if token:check "in" then
          break
        end
        token:require ","
        namelist:append(self:read():require "Name":new_auxiliary_node())
      end
      local explist = new_auxiliary_node "explist":append(self:parse_exp())
      while true do
        local token = self:read()
        if token:check "do" then
          break
        end
        token:require ","
        explist:append(self:parse_exp())
      end
      local result = token:new_statement_node "generic_for":extend {
        namelist,
        explist,
        self:parse_block(),
      }
      self:read():require "end"
      return result
    end
  elseif token:check "function" then
    local u = self:read():require "Name":new_auxiliary_node()
    while true do
      local x = self:read()
      if x:check "." then
        u = x:new_expression_node():extend {
          u,
          self:read():require "Name":new_auxiliary_node(),
        }
      elseif x:check ":" then
        u = x:new_expression_node():extend {
          u,
          self:read():require "Name":new_auxiliary_node(),
        }
        self:peek():require "("
        break
      else
        x:require "("
        self:unread()
        break
      end
    end
    return token:new_auxiliary_node():extend {
      u,
      self:parse_funcbody(),
    }
  else
    self:unread()
    local prefixexp = self:parse_prefixexp()
    if prefixexp:check("call", "self") then
      return new_statement_node "call":append(prefixexp)
    else
      prefixexp:require("Name", "index", "member")

      local token
      local varlist = new_auxiliary_node "varlist":append(prefixexp)
      while true do
        token = self:read()
        if token:check "=" then
          break
        end
        token:require ","
        local var = self:parse_prefixexp()
        varlist:append(var:require("Name", "index", "member"))
      end

      local explist = new_auxiliary_node "explist"
      while true do
        explist:append(self:parse_exp())
        if not self:read():check "," then
          self:unread()
          break
        end
      end

      return token:new_statement_node():extend { varlist, explist }
    end
  end
end

function class:parse_if(token)
  local result = token:new_statement_node():append(self:parse_exp())
  self:read():require "then"
  result:append(self:parse_block())
  local token = self:read()
  if token:check "elseif" then
    return result:append(self:parse_if(token))
  elseif token:check "else" then
    return result:append(self:parse_block())
  end
  self:unread()
  return result
end

function class:parse_retstat(token)
  local result = token:new_statement_node()

  local token = self:read()
  if token:check ";" then
    self:peek():require(stat_terminals())
    return result
  elseif token:check(stat_terminals()) then
    self:unread()
    return result
  end
  self:unread()
  result:append(self:parse_exp())

  while true do
    local token = self:read()
    if token:check ";" then
      self:peek():require(stat_terminals())
      return result
    elseif token:check(stat_terminals()) then
      self:unread()
      return result
    end
    token:require ","
    result:append(self:parse_exp())
  end
end

---@param rbp integer?
---@return dromozoa.node
function class:parse_exp(rbp)
  local left = self:parse_nud(prefixexp_nud_table)
  if left then
    left = self:parse_led(left, 0, prefixexp_led_table)
  else
    left = assert(self:parse_nud(exp_nud_table))
  end
  return self:parse_led(left, rbp or 0, exp_led_table)
end

---@param rbp integer?
---@return dromozoa.node
function class:parse_prefixexp(rbp)
  local left = assert(self:parse_nud(prefixexp_nud_table))
  return self:parse_led(left, rbp or 0, prefixexp_led_table)
end

---@param token dromozoa.token
---@return dromozoa.node
function class:parse_args(token)
  if token:check "(" then
    local result = token:new_auxiliary_node "args"
    if self:peek():check ")" then
      self:read()
      return result
    end
    result:append(self:parse_exp())
    while true do
      local token = self:read()
      if token:check ")" then
        return result
      end
      token:require ","
      result:append(self:parse_exp())
    end
  elseif token:check "{" then
    return new_auxiliary_node "args":append(self:parse_table(token))
  else
    token:require "String"
    return new_auxiliary_node "args":append(token:new_auxiliary_node())
  end
end

---@return dromozoa.node
function class:parse_funcbody()
  self:read():require "("

  local parlist = new_auxiliary_node "parlist"

  local x = self:read()
  if not x:check("...", ")") then
    while true do
      parlist:append(x:require "Name":new_auxiliary_node())
      local y = self:read()
      if y:check ")" then
        x = y
        break
      end
      y:require ","
      x = self:read()
      if x:check "..." then
        break
      end
    end
  end

  if x:check "..." then
    local u = x:new_auxiliary_node()
    local y = self:read()
    if y:check "Name" then
      u:append(y:new_auxiliary_node())
      x = self:read()
    else
      x = y
    end
    parlist:append(u)
  end

  x:require ")"

  local result = new_auxiliary_node "funcbody":extend {
    parlist,
    self:parse_block(),
  }
  self:read():require "end"

  return result
end

---@param token dromozoa.token
---@return dromozoa.node
function class:parse_table(token)
  local result = token:new_expression_node "table"
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
    local index = self:parse_exp()
    self:read():require "]"
    self:read():require "="
    return token:new_auxiliary_node "index_field":extend {
      index,
      self:parse_exp(),
    }
  elseif token:check "Name" and self:peek():check "=" then
    local index = token:new_auxiliary_node()
    self:read()
    return token:new_auxiliary_node "member_field":extend {
      index,
      self:parse_exp(),
    }
  else
    self:unread()
    return new_auxiliary_node "list_field":append(self:parse_exp())
  end
end

--=========================================================================

---@param tokens dromozoa.token[]
function class:parse(tokens)
  self.tokens = tokens
  self.index = 1
  return self:parse_block()
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
  ["["]      = { lbp = 100, fn = class.led_index },
  ["."]      = { lbp = 100, fn = class.led_member },
  ["("]      = { lbp = 100, fn = class.led_call },
  ["{"]      = { lbp = 100, fn = class.led_call },
  ["String"] = { lbp = 100, fn = class.led_call },
  [":"]      = { lbp = 100, fn = class.led_self },
}

return class
