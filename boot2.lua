function lexer(source)
  write_string(source)
end

export_start(function ()
  local source = read_all()
  lexer(source)
end)
