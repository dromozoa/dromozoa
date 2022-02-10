-- Copyright (C) 2022 Tomoyuki Fujimori <moyu@dromozoa.com>
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
-- along with dromozoa.  If not, see <http://www.gnu.org/licenses/>.

local function dump_set(set)

  -- {a,b}...
  -- なんかいい感じの文字セットを作る

  -- { a, b }
  -- ...

  local n = 0
  local b
  for byte = 0x00, 0xFF do
    if set[byte] then
      n = n + 1
      b = byte
    end
  end

  assert(n > 0)
  if n == 256 then
    return "."
  elseif n == 1 then
    return string.char(b)
  end

  if n < 256 then
    local items = {}
    local i = 0
    local a
    local b

    for byte = 0x00, 0xFF do
      if set[byte] then
        local item = items[i]
        if not item then
          i = i + 1
          items[i] = { byte, byte }
        else
          local a, b = item[1], item[2]
          if b + 1 == byte then
            item[2] = byte
          else
            i = i + 1
            items[i] = { byte, byte }
          end
        end
      end
    end

    local buffer = { "[" }

    for i = 1, #items do
      local item = items[i]
      local a, b = item[1], item[2]
      if a == b then
        buffer[#buffer + 1] = string.char(a)
      else
        buffer[#buffer + 1] = string.char(a)
        buffer[#buffer + 1] = "-"
        buffer[#buffer + 1] = string.char(b)
      end
    end

    buffer[#buffer + 1] = "]"

    return table.concat(buffer)
  end
end

local function dump_node(out, node, id)
  id = id + 1
  node.id = id
  local operator = node[1]
  if operator == "[" then
    out:write(("%d [label = \"%s\"];\n"):format(id, dump_set(node[2])))
  else
    out:write(("%d [label = \"%s\"];\n"):format(id, operator))
    for i = 2, #node do
      id = dump_node(out, node[i], id)
    end
  end
  return id
end

local function dump_edge(out, node)
  local operator = node[1]
  if operator ~= "[" then
    for i = 2, #node do
      local that = node[i]
      out:write(("%d -> %d;\n"):format(node.id, that.id))
      dump_edge(out, that)
    end
  end
end

return function (out, node)
  out:write "digraph {\n"

  dump_node(out, node, 0)
  dump_edge(out, node)

  out:write "}\n"

  return out
end
