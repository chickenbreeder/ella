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

### Maybe?

* Build a LSP server on top of this

## Things that are missing/broken

* Codegen for more complex programs
* Branching
* Scopes
* And so much more :)
