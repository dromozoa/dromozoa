return function (context) return {
[[
local _ = { ]];
context.shared_data;
[[
 }
local _ = { ]];
context.static_data;
[[
 }
return setmetatable({}, {
  __call = function ()
    local thread = coroutine.create(function ()
      local action_data = (function ()
        local _
        return { ]];
context.action_data;
[[
 }
      end)()
      while true do
        local token = coroutine.yield()
      end
    end);
    assert(thread.resume(thread))
    setmetatable({ thread = thread }, {
      __call = function (self, token)
        return assert(coroutine.resume(self.thread, token))
      end;
    })
  end;
})
]];
} end
