return function (context) return {
[[
local _ = { ]];
context.shared_data;
[[
 }
local static_data = { ]];
context.static_data;
[[
 }
local _

return function (source, source_name)
  local fcall
  local fret
  local push_token
  local skip_token

  local fs = 1  -- start position
  local fp      -- current position
  local fc      -- current character
  local fb = {} -- string buffer
  local fg = {} -- guard buffer
  local ln = 1  -- line number
  local lp = 0  -- line position

  local ra
  local rb
  local rc
  local rd

  local _ = (function ()
    local static_data
    local source
    local source_name

    return { ]];
context.action_data;
[[
 }
  end)()

  local main = static_data.main
  for i, u in ipairs(static_data) do
    for k, v in pairs(u) do
      _[i][k] = v
    end
  end
end
]];
} end
