# ella

A small programming language that can be compiled to WASM.

```
Usage: ella <COMMAND>

Commands:
  compile  Compile a file to WASM
  eval     Evaluate the generated AST
  help     Print this message or the help of the given subcommand(s)

Options:
  -h, --help  Print help
```

## Goals

* Create a language that can be compiled to WASM without any additional tooling
* Build a language server on top of this (maybe)

## Example

Executing `cargo run -- compile test/add.ella -f wat` with

```rust
fn add(a: i32, b: i32): i32 {
    return a + b;
}
```

as the input program currently yields following result:

```wasm
(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;0;) (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.add
  )
  (export "add" (func 0))
)
```

## Missing/broken

* Codegen for more complex programs
* Branching
* Scopes
* And so much more :)
