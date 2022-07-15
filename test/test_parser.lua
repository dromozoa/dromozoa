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
--
-- https://github.com/aidansteele/osx-abi-macho-file-format-reference
-- https://developers.wonderpla.net/entry/2021/03/19/105503

--[[


  lr0_items()
    



  lalr1_items()
    set_of_items, transitions = lr0_items()
    set_of_items = lalr1_kernels(set_of_items, transitions)
    for set_of_items
      lr1_closure(set_of_item)
    return set_of_items, transitions





  set_of_items, transitions = lalr1_items()
  parser, conflicts = lr1_construct_table(set_of_items, transitions)


]]



