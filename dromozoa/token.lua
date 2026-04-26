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

---@class dromozoa.token
---@field kind string
---@field subkind string?
---@field text string
---@field value string|number
---@field srcloc dromozoa.source_location
local class = {}
local metatable = {
  __index = class,
  __name = "dromozoa.token",
}

---@param kind string
---@param subkind string?
---@param text string
---@param value string|number
---@param srcloc dromozoa.source_location
---@return dromozoa.token
function class.new(kind, subkind, text, value, srcloc)
  return setmetatable({
    kind = kind,
    subkind = subkind,
    text = text,
    value = value,
    srcloc = srcloc,
  }, metatable)
end

---@param ... string
---@return dromozoa.token
function class:expect(...)
  for _, kind in ipairs { ... } do
    if self.kind == kind then
      return self
    end
  end
  error("unexpected symbol at " .. self.srcloc:to_string())
end

return class
