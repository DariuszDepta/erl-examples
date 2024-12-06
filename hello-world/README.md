# Classic "Hello world"

## Source code

### Rust

Source code in file [hello.rs](./hello.rs):

```rust
#![no_main]

#[no_mangle]
fn _start() {
    println!("Hello world");
}
```

### Erlang

Source code in file [hello.erl](./hello.erl):

```erlang
-module(hello).
-export([start/0]).

start() ->
    io:format("Hello world~n").
```

## Compilation

### Rust

```shell
$ rustc --crate-type=cdylib --target=wasm32-wasip1 -o hello.wasm hello.rs
```

Output file: `hello.wasm`, size: ~1,8 MB

Optimization:

```shell
$ wasm-opt -O4 -o helloo.wasm hello.wasm
```

Output file: `helloo.wasm`, size: ~1,5 MB

### Erlang

```shell
$ erlc hello.erl
```

Output file: `hello.beam`, size: 720 B

## Running

### Rust

In terminal:

```shell
$ wasmer run helloo.wasm
```

Output:

```text
Hello world
```

### Erlang

In terminal:

```shell
$ erl -noshell -s hello start -s init stop
```

Output:

```text
Hello world
```

or in Erlang shell:

```text
$ erl
Erlang/OTP 26 [erts-14.2.5.4] [source] [64-bit] [smp:12:12] [ds:12:12:10] [async-threads:1] [jit:ns]

Eshell V14.2.5.4 (press Ctrl+G to abort, type help(). for help)
1> c(hello).
{ok,hello}
2> hello:start().
Hello world
ok
3> q().
ok
```

## Key takeaways

- To prepare WebAssembly binary you need to install two tools: Rust compiler with wasm targets and wasmer.
- To prepare Erlang binary you need to install Erlang/OTP tools.
- In case of a simple "Hello world" program, WebAssembly binary (after optimization) is 200% bigger than Erlang binary.
