# wasmの実験用ディレクトリ

```sh
wkg wit fetch
wasm-tools component embed wit --world test1 test1.wat -o test1.wasm
wasm-tools component new test1.wasm -o test1.component.wasm
wasmtime test1.component.wasm
```

```sh
wkg wit fetch
wasm-tools component embed wit --world test2 test2.wat -o test2.wasm
wasm-tools component new test2.wasm -o test2.component.wasm
wasmtime test2.component.wasm
```

```sh
wkg wit fetch
wasm-tools component embed wit --world test3 test3.wat -o test3.wasm
wasm-tools component new test3.wasm -o test3.component.wasm
wasmtime test3.component.wasm
```
