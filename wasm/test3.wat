(module
  (import "wasi:cli/environment@0.2.6" "get-arguments"
    (func $get_arguments (param i32)))

  (import "wasi:cli/stdout@0.2.6" "get-stdout"
    (func $get_stdout (result i32)))

  (import "wasi:io/streams@0.2.6" "[resource-drop]output-stream"
    (func $drop_output_stream (param i32)))

  (import "wasi:io/streams@0.2.6" "[method]output-stream.blocking-write-and-flush"
    (func $blocking_write_and_flush (param i32 i32 i32 i32)))

  (memory (export "memory") 1)

  (data (i32.const 8) "Hello World!\0A")

  (global $stack_pointer (mut i32) (i32.const 16384))
  (global $heap_pointer (mut i32) (i32.const 32768))

  (func $new_stack
    (param $n i32)
    (result i32)
    (local $p i32)

    (global.get $stack_pointer)
    (local.tee $p)
    (local.get $n)
    (i32.add)
    (global.set $stack_pointer)

    (local.get $p)
    (return)
  )

  (func (export "cabi_realloc")
    (param $old_ptr i32)
    (param $old_size i32)
    (param $align i32)
    (param $new_size i32)
    (result i32)
    (local $mask i32)
    (local $p i32)

    (; $mask = $align - 1 ;)
    (local.get $align)
    (i32.const 1)
    (i32.sub)
    (local.set $mask)

    (; $p = ($heap_pointer + $mask) & ($mask ~ -1) ;)
    (global.get $heap_pointer)
    (local.get $mask)
    (i32.add)
    (local.get $mask)
    (i32.const -1)
    (i32.xor)
    (i32.and)
    (local.tee $p)

    (local.get $new_size)
    (i32.add)
    (global.set $heap_pointer)

    (local.get $p)
    (return)
  )

  (func (export "wasi:cli/run@0.2.6#run")
    (result i32)
    (local $sp i32)
    (local $stdout i32)
    (local $args i32)
    (local $argv i32)
    (local $argn i32)
    (local $i i32)
    (local $arg i32)
    (local $p i32)
    (local $n i32)
    (local $result i32)

    (global.get $stack_pointer)
    (local.set $sp)

    (call $get_stdout)
    (local.set $stdout)

    (; result<_, stream-error> ;)
    (i32.const 12)
    (call $new_stack)
    (local.set $result)

    (local.get $stdout)
    (i32.const 8)
    (i32.const 13)
    (local.get $result)
    (call $blocking_write_and_flush)

    (i32.const 8)
    (call $new_stack)
    (local.tee $args)
    (call $get_arguments)

    (local.get $args)
    (i32.load)
    (local.set $argv)

    (local.get $args)
    (i32.load offset=4)
    (local.set $argn)

    (block $block
      (i32.const 0)
      (local.set $i)

      (loop $loop
        (local.get $i)
        (local.get $argn)
        (i32.ge_u)
        (br_if $block)

        (; $arg = $argv + $i * 8 ;)
        (local.get $argv)
        (local.get $i)
        (i32.const 8)
        (i32.mul)
        (i32.add)
        (local.tee $arg)

        (i32.load)
        (local.set $p)

        (local.get $arg)
        (i32.load offset=4)
        (local.set $n)

        (local.get $stdout)
        (local.get $p)
        (local.get $n)
        (local.get $result)
        (call $blocking_write_and_flush)

        (local.get $stdout)
        (i32.const 20)
        (i32.const 1)
        (local.get $result)
        (call $blocking_write_and_flush)

        (local.get $i)
        (i32.const 1)
        (i32.add)
        (local.set $i)

        (br $loop)
      )
    )

    (local.get $stdout)
    (call $drop_output_stream)

    (local.get $sp)
    (global.set $stack_pointer)

    (i32.const 0)
    (return)
  )
)
