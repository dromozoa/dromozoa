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

local x = {}
local y = {}
local f1
local f2
local f3

function f1() end
function f2(a) end
function f3(a,...) end
function f4(a,b) end
function f5(a,b,...) end
function f6(...) end

function x.f1() end
function x.f2(a) end
function x.f3(a,...) end
function x.f4(a,b) end
function x.f5(a,b,...) end
function x.f6(...) end

function y:f1() end
function y:f2(a) end
function y:f3(a,...) end
function y:f4(a,b) end
function y:f5(a,b,...) end
function y:f6(...) end
