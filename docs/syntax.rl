// vim: syntax=ragel:

%%{
  machine syntax;

  Name = 'N';

  exp = 'e';
  explist_ = exp (',' exp)*;

  field_ = 'f';
  fieldsep = [,;];
  fieldlist = field_ (fieldsep field_)* fieldsep?;

  explist := explist_;
  args := '(' explist_? ')';
  tableconstructor := '{' fieldlist? '}';
  field := '[' exp ']' '=' exp | Name '=' exp | exp;
}%%
