// vim: syntax=ragel:

/*
  field = 'f';
  fieldsep = [,;];
  fieldlist = field (fieldsep field)* fieldsep?;
  main := '{' fieldlist? '}';
*/

%%{
  machine syntax;

  exp = 'e';
  explist_ = exp (',' exp)*;

  field = 'f';
  fieldsep = [,;];
  fieldlist = field (fieldsep field)* fieldsep?;

  explist := explist_;
  args := '(' explist_? ')';
  tableconstructor := '{' fieldlist? '}';
}%%
