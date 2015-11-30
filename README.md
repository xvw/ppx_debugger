# ppx_debugger

**ppx_debugger** is a small ppx-extension to enable refutable log system.
This package provide some `[@@@attributes]` for displaying output.

## Installation

todo

## Usage

### Basic log

```ocaml
(* ... some OCaml code *)
[@@@log "This is a log"]
```
Using the ppx-extension, this code, at the execution, will be display on
stdout : ` <file.ml:line> This is a log `.

### Formatted log


```ocaml
(* ... some OCaml code *)
let a = 1 and b = "log"
[@@@logf "This is %d %s", a, b]
```
Using the ppx-extension, this code, at the execution, will be display on
stdout : ` <file.ml:line> This is 1 log `.
