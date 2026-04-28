// vim: syntax=ragel:

%%{
  machine syntax;

  exp = 'e';
  explist_ = exp (',' exp)*;

  explist := explist_;
  args := '(' explist_? ')';
}%%
