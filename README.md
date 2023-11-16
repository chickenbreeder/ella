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
<sub>(Note: Compile only works for test/simple.ella right now)</sub>

## Goals

* Create a language that can be compiled to WASM without any additional tooling
* Build a LSP server on top of this (maybe)

## Example

Executing `cargo run -- compile test/add.ella --wat` with

```rust
fn add(a, b) {
    return a + b;
}
```

as the input program currently yields following result:

```wasm
(module
  (func (;0;) (type 0)
    local.get 0
    local.get 1
    i64.add
    return
  )
  (export "add" (func 0))
  (type (;0;) (func (param i64 i64) (result i64)))
)
```

## Missing/broken

* Codegen for more complex programs
* Branching
* Scopes
* And so much more :)
