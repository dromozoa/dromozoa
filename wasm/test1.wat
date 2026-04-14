(module
  (;
    - wasmtime 43.0.0のwasiは0.2.6
    - WASI/proposals/cli/wit/run.wit
      @since(version = 0.2.0)
      interface run {
        /// Run the program.
        @since(version = 0.2.0)
        run: func() -> result;
      }
    - resultはi32で正しい？
    - 関数ではなく、instanceを渡すべき？
    - cabi_reallocは必要？
  ;)
  (func (export "wasi:cli/run@0.2.6#run") (result i32)
    i32.const 1
  )
)
