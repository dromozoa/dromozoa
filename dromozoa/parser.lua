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
local function new_auxiliary_node(kind)
  return node.new("auxiliary", kind)
end

---@return string ...
local function is_stat_terminal()
  return "end", "until", "elseif", "else", "EOF"
end

---@return string ...
local function is_var()
  return "Name", "index", "member"
end

--=========================================================================

---@alias dromozoa.nud fun(parser: dromozoa.parser, x: dromozoa.token): dromozoa.node
---@alias dromozoa.led_function fun(parser: dromozoa.parser, u: dromozoa.node, x: dromozoa.token, rbp: integer): dromozoa.node
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
---@field tokens dromozoa.token[]
---@field index integer
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.parser",
}

---@return dromozoa.parser
function class.new()
  return setmetatable({
    tokens = {},
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
  error "failed to peek"
end

---@return dromozoa.token
function class:read()
  local token = self:peek()
  self.index = self.index + 1
  return token
end

function class:unread()
  for i = self.index - 1, 1, -1 do
    local token = self.tokens[i]
    if not token:check("Space", "Comment") then
      self.index = i
      return
    end
  end
  error "failed to unread"
end

--=========================================================================

---@param x dromozoa.token
---@return dromozoa.node
function class:nud_token(x)
  return x:new_expression_node()
end

---@param x dromozoa.token
---@return dromozoa.node
function class:nud_function(x)
  return x:new_expression_node():append(self:parse_funcbody())
end

---@param x dromozoa.token
---@return dromozoa.node
function class:nud_table(x)
  return self:parse_tableconstructor(x)
end

---@param x dromozoa.token
---@return dromozoa.node
function class:nud_prefix(x)
  return x:new_expression_node():append(self:parse_exp(prefix_lbp))
end

---@param x dromozoa.token
---@return dromozoa.node
function class:nud_group(x)
  local u = x:new_expression_node "group":append(self:parse_exp())
  self:read():require ")"
  return u
end

--=========================================================================

---@param u dromozoa.node
---@param x dromozoa.token
---@param rbp integer
---@return dromozoa.node
function class:led_left(u, x, rbp)
  return x:new_expression_node():extend { u, self:parse_exp(rbp) }
end

---@param u dromozoa.node
---@param x dromozoa.token
---@param rbp integer
---@return dromozoa.node
function class:led_right(u, x, rbp)
  return x:new_expression_node():extend { u, self:parse_exp(rbp - 1) }
end

---@param u dromozoa.node
---@param x dromozoa.token
---@return dromozoa.node
function class:led_index(u, x)
  local u = x:new_expression_node "index":extend { u, self:parse_exp() }
  self:read():require "]"
  return u
end

---@param u dromozoa.node
---@param x dromozoa.token
---@return dromozoa.node
function class:led_member(u, x)
  return x:new_expression_node "member":extend {
    u,
    self:read():require "Name":new_auxiliary_node(),
  }
end

---@param u dromozoa.node
---@param x dromozoa.token
---@return dromozoa.node
function class:led_call(u, x)
  return x:new_expression_node "call":extend { u, self:parse_args(x) }
end

---@param u dromozoa.node
---@param x dromozoa.token
---@return dromozoa.node
function class:led_self(u, x)
  return x:new_expression_node "self":extend {
    u,
    self:read():require "Name":new_auxiliary_node(),
    self:parse_args(self:read()),
  }
end

--=========================================================================

---@param nud_table table<string, dromozoa.nud>
---@return dromozoa.node?
---@return string?
function class:parse_nud(nud_table)
  local x = self:read()
  local nud = nud_table[x.kind]
  if not nud then
    self:unread()
    return nil, "syntax error at " .. x.srcloc:to_string()
  end
  return nud(self, x)
end

---@param u dromozoa.node
---@param rbp integer
---@param led_table table<string, dromozoa.led>
---@return dromozoa.node
function class:parse_led(u, rbp, led_table)
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

---@param kind string?
---@return dromozoa.node
function class:parse_block(kind)
  local u = new_block_node(kind or "block")
  local x
  while true do
    x = self:read()
    if x:check "return" then
      u:append(x:new_statement_node():append(self:parse_retstat()))
      x = self:read()
      break
    elseif x:check(is_stat_terminal()) then
      break
    end
    self:unread()
    u:append(self:parse_stat())
  end
  x:require(is_stat_terminal())
  self:unread()
  return u
end

---@return dromozoa.node
function class:parse_stat()
  local x = self:read()
  if x:check ";" then
    return x:new_statement_node "empty"
  elseif x:check "::" then
    local u = x:new_statement_node "label":append(self:read():require "Name":new_auxiliary_node())
    self:read():require "::"
    return u
  elseif x:check "break" then
    return x:new_statement_node()
  elseif x:check "goto" then
    return x:new_statement_node():append(self:read():require "Name":new_auxiliary_node())
  elseif x:check "do" then
    local u = x:new_statement_node():append(self:parse_block())
    self:read():require "end"
    return u
  elseif x:check "while" then
    local u = x:new_statement_node():append(self:parse_exp())
    self:read():require "do"
    u:append(self:parse_block())
    self:read():require "end"
    return u
  elseif x:check "repeat" then
    local u = x:new_statement_node():append(self:parse_block())
    self:read():require "until"
    u:append(self:parse_exp())
    return u
  elseif x:check "if" then
    local u = self:parse_if(x)
    self:read():require "end"
    return u
  elseif x:check "for" then
    local y = self:read():require "Name"
    if self:peek():check "=" then
      return self:parse_numeric_for(x, y)
    else
      return self:parse_generic_for(x, y)
    end
  elseif x:check "function" then
    return x:new_statement_node():extend {
      self:parse_funcname(),
      self:parse_funcbody(),
    }
  elseif x:check "local" or x:check "global" then
    if self:peek():check "function" then
      self:read()
      return x:new_statement_node(x.kind .. "_function"):extend {
        self:read():require "Name":new_auxiliary_node(),
        self:parse_funcbody(),
      }
    else
      local u = x:new_statement_node():append(self:parse_declaration(x.kind))
      if self:peek():check "=" then
        self:read()
        u:append(self:parse_explist())
      end
      return u
    end
  else
    self:unread()
    local u = self:parse_prefixexp()
    if u:check("call", "self") then
      return u.token:new_statement_node "call":append(u)
    else
      return self:parse_assignment(u)
    end
  end
end

---@param x dromozoa.token
---@return dromozoa.node
function class:parse_if(x)
  local u = x:new_statement_node "if":append(self:parse_exp())
  self:read():require "then"
  u:append(self:parse_block())

  local x = self:read()
  if x:check "elseif" then
    return u:append(new_block_node "block":append(self:parse_if(x)))
  elseif x:check "else" then
    return u:append(self:parse_block())
  end
  x:require "end"
  self:unread()
  return u
end

---@param x dromozoa.token
---@param y dromozoa.token
---@return dromozoa.node
function class:parse_numeric_for(x, y)
  self:read():require "="
  local u = new_auxiliary_node "expressions"
  u:append(self:parse_exp())
  self:read():require ","
  u:append(self:parse_exp())
  local z = self:read()
  if z:check "," then
    u:append(self:parse_exp())
    z = self:read()
  end
  z:require "do"

  local u = x:new_statement_node "numeric_for":extend {
    y:new_auxiliary_node(),
    u,
    self:parse_block(),
  }
  self:read():require "end"
  return u
end

---@param x dromozoa.token
---@param y dromozoa.token
---@return dromozoa.node
function class:parse_generic_for(x, y)
  local u = new_auxiliary_node "names":append(y:new_auxiliary_node())
  while true do
    local x = self:read()
    if x:check "in" then
      break
    end
    x:require ","
    u:append(self:read():require "Name":new_auxiliary_node())
  end
  local v = self:parse_explist()
  self:read():require "do"

  local u = x:new_statement_node "generic_for":extend {
    u,
    v,
    self:parse_block(),
  }
  self:read():require "end"
  return u
end

---@param kind "global" | "local"
---@return dromozoa.node
function class:parse_declaration(kind)
  local attribute

  local x = self:read()
  if x:check "<" then
    attribute = self:read():require "Name"
    self:read():require ">"
    x = self:read()
  end

  if kind == "global" and x:check "*" then
    local u = x:new_auxiliary_node "any"
    u.attribute = attribute
    return u
  end

  local u = new_auxiliary_node("names")
  u.attribute = attribute
  while true do
    local v = x:require "Name":new_auxiliary_node()
    u:append(v)
    x = self:read()
    if x:check "<" then
      v.attribute = self:read():require "Name"
      self:read():require ">"
      x = self:read()
    end
    if not x:check "," then
      self:unread()
      break
    end
    x = self:read()
  end
  return u
end

---@param u dromozoa.node
---@return dromozoa.node
function class:parse_assignment(u)
  local u = new_auxiliary_node "variables":append(u:require(is_var()))

  local x
  while true do
    x = self:read()
    if x:check "=" then
      break
    end
    x:require ","
    u:append(self:parse_prefixexp():require(is_var()))
  end
  x:require "="

  return x:new_statement_node "assignment":extend { u, self:parse_explist() }
end

---@return dromozoa.node
function class:parse_retstat()
  local u
  if self:peek():check(";", is_stat_terminal()) then
    u = new_auxiliary_node "expressions"
  else
    u = self:parse_explist()
  end
  local x = self:read()
  if x:check ";" then
    x = self:read()
  end
  x:require(is_stat_terminal())
  self:unread()
  return u
end

---@return dromozoa.node
function class:parse_funcname()
  local u = self:read():require "Name":new_expression_node()
  local x
  while true do
    x = self:read()
    if x:check "(" then
      break
    elseif x:check ":" then
      u = x:new_expression_node "method":extend {
        u,
        self:read():require "Name":new_auxiliary_node(),
      }
      x = self:read()
      break
    end
    x:require "."
    u = x:new_expression_node "member":extend {
      u,
      self:read():require "Name":new_auxiliary_node(),
    }
  end
  x:require "("
  self:unread()
  return u
end

---@return dromozoa.node
function class:parse_explist()
  local u = new_auxiliary_node "expressions"
  while true do
    u:append(self:parse_exp())
    if not self:read():check "," then
      self:unread()
      break
    end
  end
  return u
end

---@param rbp integer?
---@return dromozoa.node
function class:parse_exp(rbp)
  local u = self:parse_nud(prefixexp_nud_table)
  if u then
    u = self:parse_led(u, 0, prefixexp_led_table)
  else
    u = assert(self:parse_nud(exp_nud_table))
  end
  return self:parse_led(u, rbp or 0, exp_led_table)
end

---@param rbp integer?
---@return dromozoa.node
function class:parse_prefixexp(rbp)
  local u = assert(self:parse_nud(prefixexp_nud_table))
  return self:parse_led(u, rbp or 0, prefixexp_led_table)
end

---@param x dromozoa.token
---@return dromozoa.node
function class:parse_args(x)
  if x:check "(" then
    local u
    if self:peek():check ")" then
      u = new_auxiliary_node "expressions"
    else
      u = self:parse_explist()
    end
    self:read():require ")"
    return u
  elseif x:check "{" then
    return new_auxiliary_node "expressions":append(self:parse_tableconstructor(x))
  else
    x:require "String"
    return new_auxiliary_node "expressions":append(x:new_expression_node())
  end
end

---@return dromozoa.node
function class:parse_funcbody()
  local u = new_auxiliary_node "parameters"

  self:read():require "("
  local x = self:read()
  if not x:check("...", ")") then
    while true do
      u:append(x:require "Name":new_auxiliary_node())
      x = self:read()
      if x:check ")" then
        break
      end
      x:require ","
      x = self:read()
      if x:check "..." then
        break
      end
    end
  end
  if x:check "..." then
    local v = x:new_auxiliary_node()
    u:append(v)
    x = self:read()
    if x:check "Name" then
      v:append(x:new_auxiliary_node())
      x = self:read()
    end
  end
  x:require ")"

  local u = new_auxiliary_node "body":extend {
    u,
    self:parse_block(),
  }
  self:read():require "end"
  return u
end

---@param x dromozoa.token
---@return dromozoa.node
function class:parse_tableconstructor(x)
  local u = x:require "{":new_expression_node "table"
  while true do
    x = self:read()
    if x:check "}" then
      break
    end
    u:append(self:parse_field(x))
    x = self:read()
    if x:check "}" then
      break
    end
    x:require(",", ";")
  end
  x:require "}"
  return u
end

---@param x dromozoa.token
---@return dromozoa.node
function class:parse_field(x)
  if x:check "[" then
    local u = self:parse_exp()
    self:read():require "]"
    return self:read():require "=":new_auxiliary_node "index_field":extend {
      u,
      self:parse_exp(),
    }
  elseif x:check "Name" and self:peek():check "=" then
    return self:read():new_auxiliary_node "member_field":extend {
      x:new_auxiliary_node(),
      self:parse_exp(),
    }
  else
    self:unread()
    return new_auxiliary_node "list_field":append(self:parse_exp())
  end
end

--=========================================================================

---@param tokens dromozoa.token[]
---@return dromozoa.node
function class:parse(tokens)
  self.tokens = tokens
  self.index = 1
  local u = self:parse_block "chunk"
  self:peek():require "EOF"
  return u
end

--=========================================================================

exp_nud_table = {
  ["nil"]      = class.nud_token,
  ["false"]    = class.nud_token,
  ["true"]     = class.nud_token,
  ["Float"]    = class.nud_token,
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
