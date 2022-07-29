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
_ = nil

return function (source, source_name)
  local action_data = (function ()
    local static_data
    local source
    local source_name
    return { ]];
context.action_data;
[[
 }
  end)()
end
]];
} end
