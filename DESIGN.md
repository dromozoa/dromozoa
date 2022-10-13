# dromozoa

Luaに型付けを行うために、以下の制約を考える。
1. requireを静的に解決する。
2. chunkレベルで解析を行う。
3. coroutineとerrorは使わない。
