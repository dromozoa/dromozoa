// vim: syntax=ragel:
// ragel -p -V syntax.rl -o /dev/stdout | grep -v '^[[:space:]]*ENTRY' | dot -Tsvg >syntax.svg
%%{
  machine syntax;

  Name = 'N';

  stat = 's';
  retstat_ = 'r';
  end = '$';

  attrib = 'a';

  var = 'v';
  varlist = var (',' var)*;

  exp = 'e';
  explist_ = exp (',' exp)*;

  varargparam = '.' Name?;
  parlist = (Name (',' Name)*) (',' varargparam)? | varargparam;

  field_ = 'f';
  fieldsep = [,;];
  fieldlist = field_ (fieldsep field_)* fieldsep?;

  block := stat* retstat_? end;
  attnamelist := attrib? Name attrib? (',' Name attrib?)*;
  retstat := explist_? ';'? end;
  funcname := Name ('.' Name)* (':' Name)?;
  assign := varlist '=' explist_;
  explist := explist_;
  args := '(' explist_? ')';
  params := '(' parlist? ')';
  tableconstructor := '{' fieldlist? '}';
  field := '[' exp ']' '=' exp | Name '=' exp | exp;
}%%
