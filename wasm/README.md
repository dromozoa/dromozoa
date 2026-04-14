# wasmの実験用ディレクトリ

```sh
wkg wit fetch
wasm-tools component embed wit --world dromozoa:test1/command test1.wat -o test1.wasm
wasm-tools component new test1.wasm -o test1.component.wasm
wasmtime test1.component.wasm
```
