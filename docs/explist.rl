// vim: syntax=ragel:

%%{
  machine explist;
  exp = 'e';
  explist = exp (',' exp)*;
  main := explist;
}%%
