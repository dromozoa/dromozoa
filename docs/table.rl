// vim: syntax=ragel:

%%{
  machine table;
  field = 'f';
  fieldsep = [,;];
  fieldlist = field (fieldsep field)* fieldsep?;
  main := '{' fieldlist? '}';
}%%
