(module
  (import "wasi:cli/stdout@0.2.6" "get-stdout"
    (func $get_stdout (result i32)))

  (import "wasi:io/streams@0.2.6" "[resource-drop]output-stream"
    (func $drop_output_stream (param i32)))

  (import "wasi:io/streams@0.2.6" "[method]output-stream.blocking-write-and-flush"
    (func $blocking_write_and_flush (param i32 i32 i32 i32)))

  (memory (export "memory") 1)

  (data (i32.const 8) "Hello World!\0A")

  (func (export "wasi:cli/run@0.2.6#run") (result i32)
    (local $stdout i32)

    (call $get_stdout)
    (local.set $stdout)

    (local.get $stdout)
    (i32.const 8)
    (i32.const 13)
    (i32.const 256) (; 結果をどっかに書いておく ;)
    (call $blocking_write_and_flush)

    (local.get $stdout)
    (call $drop_output_stream)

    (i32.const 0)
  )
)
