// vim: syntax=ragel:

%%{
  machine args;
  exp = 'e';
  explist = exp (',' exp)*;
  main := '(' explist? ')';
}%%
