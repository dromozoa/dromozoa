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

local array = require "dromozoa.array"
local regexp = {
  pattern = require "dromozoa.regexp.pattern";
  machine = require "dromozoa.regexp.machine";
  compile = require "dromozoa.regexp.compile";
}
local parser = {
  grammar = require "dromozoa.parser.grammar";
  lalr = require "dromozoa.parser.lalr";
  compile = require "dromozoa.parser.compile";
}

local regexp_filename, parser_filename = ...
local token_names = array()

local _ = regexp.pattern

local out = assert(io.open(regexp_filename, "w"))
out:write(regexp.compile {
  "local ra";

  long_comment = regexp.machine.guard("freturn()", {
    _"\n"/"ln=ln+1 lp=fp" + _"\r"/"lp=fp"*"?";
    _"\r"/"ln=ln+1 lp=fp" + _"\n"/"lp=fp"*"?";
    _(_);
  });

  long_literal_string = regexp.machine.guard("freturn()", {
    _"\n"/"append(0x0A) ln=ln+1 lp=fp" + _"\r"/"lp=fp"*"?";
    _"\r"/"append(0x0A) ln=ln+1 lp=fp" + _"\n"/"lp=fp"*"?";
    _(_)/"append(fc)";
  });

  short_literal_string = regexp.machine.guard("freturn()", {
    _"\\" + _{
      _"a"/"append(0x07)";
      _"f"/"append(0x0C)";
      _"n"/"append(0x0A)";
      _"r"/"append(0x0D)";
      _"t"/"append(0x09)";
      _"v"/"append(0x0B)";
      _"\\"/"append(fc)";
      _"\""/"append(fc)";
      _"\'"/"append(fc)";
      _"\n"/"append(0x0A) ln=ln+1 lp=fp" + _"\r"/"lp=fp"*"?";
      _"\r"/"append(0x0A) ln=ln+1 lp=fp" + _"\n"/"lp=fp"*"?";
      _"z" + _{
        _"\n"/"ln=ln+1 lp=fp" + _"\r"/"lp=fp"*"?";
        _"\r"/"ln=ln+1 lp=fp" + _"\n"/"lp=fp"*"?";
        _{" \f\t\v"}*"+"
      }*"*";
      _"x" + _{
        _["09"]/"ra=fc-${<0>}";
        _["af"]/"ra=fc-${<a>}+10";
        _["AF"]/"ra=fc-${<A>}+10";
      } + _{
        _["09"]/"append(ra*16+fc-${<0>})";
        _["af"]/"append(ra*16+fc-${<a>}+10)";
        _["AF"]/"append(ra*16+fc-${<A>}+10)";
      };
      _"u" + _"{"/"ra=0" + _{
        _["09"]/"ra=ra*16+fc-${<0>}";
        _["af"]/"ra=ra*16+fc-${<a>}+10";
        _["AF"]/"ra=ra*16+fc-${<A>}+10";
      }*"+" + _"}"/"fassert(ra<=0x7FFFFFFF,'UTF-8 value too large') append_unicode(ra)";
    };

    (_"\\" + _["09"]/"ra=fc-${<0>}" + _["09"]/"ra=ra*10+fc-${<0>}"*{0,2}) %"fassert(ra<=255,'decimal escape too large') append(ra)";

    _(_)/"append(fc)";
  });

  regexp.machine.lexer(token_names, {
    _{
      _"\n"/"ln=ln+1 lp=fp" + _"\r"/"lp=fp"*"?";
      _"\r"/"ln=ln+1 lp=fp" + _"\n"/"lp=fp"*"?";
      _{" \f\t\v"}*"+";
    }*"+";

    (_"--" + _"["/"guard_clear(${<]>})" + (_"="/"guard_append(fc)")*"*" + _"["/"guard_append(${<]>})") %"fcall($long_comment) push()";

    _"--" + -_{"\n\r"}*"*";

    LongLiteralString = (_"["/"guard_clear(${<]>})" + (_"="/"guard_append(fc)")*"*" + _"["/"guard_append(${<]>})" + _{
      _"\n"/"ln=ln+1 lp=fp" + _"\r"/"lp=fp"*"?";
      _"\r"/"ln=ln+1 lp=fp" + _"\n"/"lp=fp"*"?";
    }*"?") %"clear() fcall($long_literal_string) push(true)";

    ShortLiteralString = _{"\'\""}/"guard_clear(fc)" %"clear() fcall($short_literal_string) push(true)";

    -- 8進数は存在しないのでleading zerosが許容される。
    DecimalIntegerNumeral = _["09"]*"+";

    -- C言語のリテラルのdecimal-floating-constantに類似しているが、以下の点で異
    -- なる。
    -- 1. 小数点も指数部もない場合はDecimalIntegerNumeralがマッチするので除外し
    --    ない。
    -- 2. 接尾辞は持たない。
    DecimalFloatingNumeral = _{
      _["09"]*"*" + _"." + _["09"]*"+";
      _["09"]*"+" + _"."*"?";
    } + (_{"eE"} + _{"+-"}*"?" + _["09"]*"+")*"?";

    HexadecimalIntegerNumeral = _"0" + _{"xX"} + _["09AFaf"]*"+";

    -- C言語のリテラルのhexadecimal-floating-constantに類似しているが、以下の点
    -- で異なる。
    -- 1. 指数部を省略できる。C言語のリテラルでは指数を省略できないが、strtodで
    --    は省略できる。
    -- 2. 小数点も指数部もない場合はHexadecimalIntegerNumeralがマッチするので除
    --    外しない。
    -- 3. 接尾辞は持たない。
    HexadecimalFloatingNumeral = _"0" + _{"xX"} + _{
      _["09AFaf"]*"*" + _"." + _["09AFaf"]*"+";
      _["09AFaf"]*"+" + _"."*"?";
    } + (_{"pP"} + _{"+-"}*"?" + _["09"]*"+")*"?"
    ;

    _"and";      _"break";    _"do";       _"else";     _"elseif";   _"end";
    _"false";    _"for";      _"function"; _"goto";     _"if";       _"in";
    _"local";    _"nil";      _"not";      _"or";       _"repeat";   _"return";
    _"then";     _"true";     _"until";    _"while";

    _"+";   _"-";   _"*";   _"/";   _"%";   _"^";   _"#";
    _"&";   _"~";   _"|";   _"<<";  _">>";  _"//";
    _"==";  _"~=";  _"<=";  _">=";  _"<";   _">";   _"=";
    _"(";   _")";   _"{";   _"}";   _"[";   _"]";   _"::";
    _";";   _":";   _",";   _".";   _"..";  _"...";

    Name = _["AZaz_"] + _["09AZaz_"]*"*";
  });
})
out:close()

local _ = parser.grammar.body
local expect = parser.grammar.expect
local left = parser.grammar.left
local right = parser.grammar.right

-- 抽象構文木を愚直に作成する。
local grammar, actions, conflictions, data = parser.lalr(parser.grammar(token_names, {
  expect(3);

  left "or";
  left "and";
  left "<" ">" "<=" ">=" "~=" "==";
  left "|";
  left "~";
  left "&";
  left "<<" ">>";
  right "..";
  left "+" "-";
  left "*" "/" "//" "%";
  right "not" "#" "UNM" "BNOT";
  right "^";

  chunk = _"block";

  block
    = _"block_" %"$$=$1"
    + _"block_" "retstat" %"$$=$1 append($2)";

  block_
    = _ %"$$=create($block)"
    + _"block_" ";" %"$$=$1"
    + _"block_" "stat" %"$$=$1 append($2)";

  stat
    = _"varlist" "=" "explist" %"$$=$0 append($3,$2,$1)"
    + _"functioncall"
    + _"label"
    + _"break"
    + _"goto" "Name"
    + _"do" "block" "end"
    + _"while" "exp" "do" "block" "end"
    + _"repeat" "block" "until" "exp"
    + _"if" "exp" "then" "block" "elseif_exp_then_block" "else_block" "end"
    + _"for" "Name" "=" "exp,exp[,exp]" "do" "block" "end" %"$$=$0 append($1,$4,$3,$2,$5,$6,$7)"
    + _"for" "namelist" "in" "explist" "do" "block" "end" %"$$=$0 append($1,$4,$3,$2,$5,$6,$7)"
    + _"function" "funcname" "funcbody"
    + _"local" "function" "Name" "funcbody"
    + _"local" "attnamelist"
    + _"local" "attnamelist" "=" "explist" %"$$=$0 append($1,$4,$3,$2)";

  elseif_exp_then_block
    = _
    + _"elseif_exp_then_block" "elseif" "exp" "then" "block";

  else_block
    = _
    + _"else" "block";

  ["exp,exp[,exp]"]
    = _"exp" "," "exp" %"$$=create($explist) append($1, $3)"
    + _"exp" "," "exp" "," "exp" %"$$=create($explist) append($1, $3, $5)";

  attnamelist
    = _"Name" "attrib"
    + _"attnamelist" "," "Name" "attrib";

  attrib
    = _
    + _"<" "Name" ">";

  retstat
    = _"return"
    + _"return" ";" %"$$=$0 append($1)"
    + _"return" "explist"
    + _"return" "explist" ";" %"$$=$0 append($1,$2)";

  label = _"::" "Name" "::";

  funcname
    = _"funcname_" %"$$=$1"
    + _"funcname_" ":" "Name";

  funcname_
    = _"Name" %"$$=create($funcname) append($1)"
    + _"funcname_" "." "Name" %"$$=create($funcname) append($1,$2,$3)";

  varlist
    = _"var" %"$$=create($varlist) append($1)"
    + _"varlist" "," "var" %"$$=$1 append($3)";

  var
    = _"Name"
    + _"prefixexp" "[" "exp" "]"
    + _"prefixexp" "." "Name"
    + _"functioncall" "[" "exp" "]"
    + _"functioncall" "." "Name";

  namelist
    = _"Name" %"$$=create($namelist) append($1)"
    + _"namelist" "," "Name" %"$$=$1 append($3)";

  explist
    = _"exp" %"$$=create($explist) append($1)"
    + _"explist" "," "exp" %"$$=$1 append($3)";

  exp
    = _"nil"
    + _"false"
    + _"true"
    + _"Numeral"
    + _"LiteralString"
    + _"..."
    + _"functiondef"
    + _"prefixexp"
    + _"functioncall"
    + _"tableconstructor"
    -- binop
    + _"exp" "+" "exp"
    + _"exp" "-" "exp"
    + _"exp" "*" "exp"
    + _"exp" "/" "exp"
    + _"exp" "//" "exp"
    + _"exp" "^" "exp"
    + _"exp" "%" "exp"
    + _"exp" "&" "exp"
    + _"exp" "~" "exp"
    + _"exp" "|" "exp"
    + _"exp" ">>" "exp"
    + _"exp" "<<" "exp"
    + _"exp" ".." "exp"
    + _"exp" "<" "exp"
    + _"exp" "<=" "exp"
    + _"exp" ">" "exp"
    + _"exp" ">=" "exp"
    + _"exp" "==" "exp"
    + _"exp" "~=" "exp"
    + _"exp" "and" "exp"
    + _"exp" "or" "exp"
    -- unop
    + _"-" "exp" :prec "UNM"
    + _"not" "exp"
    + _"#" "exp"
    + _"~" "exp" :prec "BNOT";

  -- The Complete Syntax of LuaのEBNFは、prefixexpとfunctioncallが相互に依存し
  -- ている。そのまま利用するとshift/shift競合が発生する。これを回避するため、
  -- prefixexpを参照する箇所にfunctioncallを展開する。
  prefixexp
    = _"var"
    + _"(" "exp" ")";

  functioncall
    = _"prefixexp" "args"
    + _"prefixexp" ":" "Name" "args"
    + _"functioncall" "args"
    + _"functioncall" ":" "Name" "args";

  args
    = _"(" ")"
    + _"(" "explist" ")"
    + _"tableconstructor"
    + _"LiteralString";

  functiondef = _"function" "funcbody";

  funcbody
    = _"(" ")" "block" "end"
    + _"(" "parlist" ")" "block" "end";

  parlist
    = _"namelist"
    + _"namelist" "," "..."
    + _"...";

  tableconstructor
    = _"{" "}"
    + _"{" "fieldlist" "}";

  fieldlist
    = _"fieldlist_" %"$$=$1"
    + _"fieldlist_" "fieldsep" %"$$=$1";

  fieldlist_
    = _"field" %"$$=create($fieldlist) append($1)"
    + _"fieldlist_" "fieldsep" "field" %"$$=$1 append($3)";

  field
    = _"[" "exp" "]" "=" "exp" %"$$=$0 append($5,$4,$1,$2,$3)"
    + _"Name" "=" "exp" %"$$=$0 append($3,$2,$1)"
    + _"exp";

  fieldsep
    = _","
    + _";";

  LiteralString
    = _"LongLiteralString"
    + _"ShortLiteralString";

  Numeral
    = _"DecimalIntegerNumeral"
    + _"DecimalFloatingNumeral"
    + _"HexadecimalIntegerNumeral"
    + _"HexadecimalFloatingNumeral";
}))

for _, message in conflictions:ipairs() do
  if message:find "^%[warn%]" then
    print(message)
  end
end

local out = assert(io.open(parser_filename, "w"))
out:write(parser.compile(grammar, actions))
out:close()
