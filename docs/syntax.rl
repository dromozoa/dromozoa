// vim: syntax=ragel:
// ragel -p -V syntax.rl -o /dev/stdout | grep -v '^[[:space:]]*ENTRY' | dot -Tsvg >syntax.svg
%%{
  machine syntax;

  Name = 'N';

  var = 'v';
  varlist = var (',' var)*;

  exp = 'e';
  explist_ = exp (',' exp)*;

  field_ = 'f';
  fieldsep = [,;];
  fieldlist = field_ (fieldsep field_)* fieldsep?;

  assign := varlist '=' explist_;
  explist := explist_;
  args := '(' explist_? ')';
  tableconstructor := '{' fieldlist? '}';
  field := '[' exp ']' '=' exp | Name '=' exp | exp;
}%%
