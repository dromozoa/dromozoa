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
-- Under Section 7 of GPL version 3, you are granted additional
-- permissions described in the GCC Runtime Library Exception, version
-- 3.1, as published by the Free Software Foundation.
--
-- You should have received a copy of the GNU General Public License
-- and a copy of the GCC Runtime Library Exception along with
-- dromozoa.  If not, see <http://www.gnu.org/licenses/>.

--{static data section}

return function (s)
  -- function declarations
  local fcall
  local fret

  -- readonly variables
  local current_position = 0
  local current_byte

--{local data section}

--[====[


  -- temporary variables
  -- local integer_value
  -- local buffer
  -- local guard

  --
  -- code here
  --

  -- private variables
  local stack_top = 0
  local stack = {}
  local current_fsm
  local current_state
  local start_position

  fcall = function (fsm)
    stack_top = stack_top + 1
    stack[stack_top] = {
      current_fsm = current_fsm;
      current_state = current_state;
      start_position  = start_position;
    }

    current_fsm = fsm
    current_state = data[current_fsm].start_state
    start_position = current_position
  end

  fgoto = function (fsm)
    if fsm then
      current_fsm = fsm
    end
    current_state = data[current_fsm].start_state
    start_position = current_position
  end

  fret = function ()
    local item = stack[stack_top]
    current_fsm = item.current_fsm
    current_state = item.current_state
    start_position = item.start_position

    stack[stack_top] = nil
    stack_top = stack_top - 1
  end

  while true do
    current_position = current_position + 1
    current_byte = s:byte(current_position)
    if not current_byte then
      -- eof
      break
    end
    local state = data[current_fsm].transitions[current_byte][current_state]
    if state == 0 then
      -- accept or error
      if current_state <= data[current_fsm].max_accept_state then
        local accept = data[current_fsm].accept_actions[current_state]
        if accept then
        end
      else
        error ""
      end
    else
    end

  end















  local assign_byte
  local assign_range
  local append_byte
  local append_range

  local fc
  local buffer
  local guard

  local data = ...
  local stack_top = 0
  local stack = {}

  local name = main
  local ps = 1
  local p = ps
  local cs = _data[name].start_state

  fret = function ()
    local stack_item = stack[stack_top]
    name = stack_item.name
    sdata = _sdata[name]
    ldata = _ldata[name]
    ps = stack_item.ps
    cs = stack_item.cs
    stack[stack_top] = nil
    stack_top = stack_top - 1
  end



  local _local_data_section = ...

  while true do
    local byte = s:byte(p)
    if not byte then
      -- eof
      break
    end
    local state = _current_sdata.transitions[byte][_current_state]
    if state == 0 then
      if _current_state <= _current_sdata.max_accept_state then
        local accept = _local_data_section[_current_index].accept_actions[_current_state]
        if accept then
          accept()
        end
      else
        error "regexp error"
      end
    else
      if state > _static_data_section[cf].max_state then
        local t  = ns - _static_data_section[cf].max_state
        ns = transition_to_states[t]
        transition_action[n]()
      end
      cs = ns
      p = p + 1
    end
  end
]====]
end
