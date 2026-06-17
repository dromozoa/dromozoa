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

---@alias dromozoa.token_stream.lex fun(matcher: dromozoa.matcher): dromozoa.token

---@class dromozoa.token_stream
---@field lex dromozoa.token_stream.lex
---@field matcher dromozoa.matcher
---@field tokens dromozoa.token[]
---@field index integer
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.token_stream",
}

---@param lex dromozoa.token_stream.lex
---@param matcher dromozoa.matcher
---@return dromozoa.token_stream
function class.new(lex, matcher)
  return setmetatable({
    lex = lex,
    matcher = matcher,
    tokens = {},
    index = 1,
  }, metatable)
end

---@return dromozoa.token
function class:peek()
  local n = #self.tokens

  for i = self.index, n do
    local token = self.tokens[i]
    if not token:check_kind("Space", "Comment") then
      self.index = i
      return token
    end
  end

  if n > 0 and self.tokens[n]:check_kind "EOF" then
    error "cannot read past end of stream"
  end

  local i = n + 1
  while true do
    local token = self.lex(self.matcher)
    self.tokens[i] = token
    if not token:check_kind("Space", "Comment") then
      self.index = i
      return token
    end
    i = i + 1
  end
end

---@return dromozoa.token
function class:read()
  local token = self:peek()
  self.index = self.index + 1
  return token
end

function class:unread()
  for i = self.index - 1, 1, -1 do
    if not self.tokens[i]:check_kind("Space", "Comment") then
      self.index = i
      return
    end
  end
  error "cannot unread before beginning of stream"
end

return class
