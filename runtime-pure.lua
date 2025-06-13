function read_all()
  return io.read("*a")
end

function write_string(s)
  io.write(s)
end

function export_start(f)
  f()
end
