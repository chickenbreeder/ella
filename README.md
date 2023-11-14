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

Executing `cargo run -- compile test/simple.ella --wat` with

```rust
fn main() {
    let a = 4;
    let b = 42;
}
```

as the input program currently yields following result:

```wasm
(module
  (func (;0;) (type 0)
    (local i64)
    i64.const 4
    local.set 0
    i64.const 42
    local.set 1
  )
  (export "main" (func 0))
)
```

## Missing/broken

* Codegen for more complex programs
* Branching
* Scopes
* And so much more :)
