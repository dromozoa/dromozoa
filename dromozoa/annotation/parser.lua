local main = function (static_data)
  local create
  local append
  local append_unpack
  local S
  local SS
  local action_data = (function ()
    
    return { function()
end;
function()SS=S[1]
end;
function()SS=S[2] append(S[1],S[3])
end;
function()SS=create(12)
end;
function()SS=S[2]
end;
function()SS=create(12) append(S[1])
end;
function()SS=S[1] append(S[3])
end;
 }
  end)()
  local error = error
  local select = select
  local table_unpack = table.unpack or unpack
  local symbol_names = static_data.symbol_names
  local actions = static_data.actions
  local max_state = #actions
  local heads = static_data.heads
  local sizes = static_data.sizes
  local semantic_actions = static_data.semantic_actions
  local stack = { 1 }
  local nodes = { false }
  local n = 1
  function create(symbol)
    return { [0] = symbol }
  end
  function append(...)
    for i = 1, select("#", ...) do
      SS[#SS + 1] = select(i, ...)
    end
  end
  function append_unpack(...)
    for i = 1, select("#", ...) do
      append(table_unpack((select(i, ...))))
    end
  end
  return function (token)
    local symbol = token[0]
    if symbol then
      while true do
        local state = stack[n]
        local action = actions[state][symbol]
        if action == 0 then
          if token.f and token.n and token.c and token.s then
            error(token.f..":"..token.n..":"..token.c..": parser error (cannot transition near '"..token.s.."')")
          else
            error("parser error (cannot transition near '"..symbol_names[symbol].."')")
          end
        end
        if action <= max_state then
          n = n + 1
          stack[n] = action
          nodes[n] = token
          break
        end
        local index = action - max_state
        if index == 1 then
          local node = nodes[n]
          n = n - 1
          return node
        end
        local head = heads[index]
        local size = sizes[index]
        S = { [0] = { [0] = head } }
        SS = { [0] = head }
        local sf, si, sj, sn, sc
        n = n - size
        for i = 1, size do
          local s = nodes[n + i]
          S[i] = s
          SS[i] = s
          if not sf then
            sf, si, sj, sn, sc = s.f, s.i, s.j, s.n, s.c
          elseif sf == s.f then
            if not si or si > s.i then
              si = s.i
            end
            if not sj or sj < s.j then
              sj = s.j
            end
            if not sn or sn > s.n or (sn == s.n and (not sc or sc > s.c)) then
              sn, sc = s.n, s.c
            end
          end
        end
        action_data[semantic_actions[index]]()
        SS.f, SS.i, SS.j, SS.n, SS.c = sf, si, sj, sn, sc
        local state = stack[n]
        n = n + 1
        stack[n] = actions[state][head]
        nodes[n] = SS
      end
    end
  end
end
local static_data = { symbol_names={"i32","i64","f32","f64","(",")",",","->","$","type'","type","tuple","tuplelist",};
max_terminal_symbol=9;
actions={
{3,4,5,6,8,0,0,0,0,0,2,7,0};
{0,0,0,0,0,0,0,0,17,0,0,0,0};
{0,0,0,0,0,18,18,0,18,0,0,0,0};
{0,0,0,0,0,19,19,0,19,0,0,0,0};
{0,0,0,0,0,20,20,0,20,0,0,0,0};
{0,0,0,0,0,21,21,0,21,0,0,0,0};
{0,0,0,0,0,0,0,9,0,0,0,0,0};
{3,4,5,6,8,10,0,0,0,0,12,7,11};
{0,0,0,0,8,0,0,0,0,0,0,13,0};
{0,0,0,0,0,23,23,23,23,0,0,0,0};
{0,0,0,0,0,14,15,0,0,0,0,0,0};
{0,0,0,0,0,25,25,0,0,0,0,0,0};
{0,0,0,0,0,22,22,0,22,0,0,0,0};
{0,0,0,0,0,24,24,24,24,0,0,0,0};
{3,4,5,6,8,0,0,0,0,0,16,7,0};
{0,0,0,0,0,26,26,0,0,0,0,0,0};
};
heads={10,11,11,11,11,11,12,12,13,13,};
sizes={1,1,1,1,1,3,2,3,1,3,};
semantic_actions={1,2,2,2,2,3,4,5,6,7,};
 }
return setmetatable({}, {
  __index = static_data;
  __call = function (static_data)
    return main(static_data)
  end;
})
