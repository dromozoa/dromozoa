# wasmの実験用ディレクトリ

```sh
wkg wit fetch
wasm-tools component embed wit --world test1 test1.wat -o test1.embedded.wasm
wasm-tools component new test1.embedded.wasm -o test1.component.wasm
wasmtime test1.component.wasm
```
