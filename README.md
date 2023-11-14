# ella

```
Usage: ella <COMMAND>

Commands:
  compile  Compile a file to WASM
  eval     Evaluate the generated AST
  help     Print this message or the help of the given subcommand(s)

Options:
  -h, --help  Print help
```
<small>(Note: Compile only works for <u>test/simple.ella</u> right now)</small>

## Goals

* Create a small language that can be compiled to WASM without any additional tooling

### Maybe?

* Build an LSP server on top of this

## Things that are missing/broken

* Codegen for more complex programs
* Branching
* Scopes
* And so much more :)
