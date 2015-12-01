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

The log could be formatted :

```ocaml
(* ... some OCaml code *)
let a = 1 and b = "log"
[@@@log "This is %d %s", a, b]
```
Using the ppx-extension, this code, at the execution, will be display on
stdout : ` <file.ml:line> This is 1 log `.

### Breakpoint
```ocaml
(* ... some OCaml code *)
[@@@breakpoint]
```
Wait for [ENTER] press to process the nextstep

### Attach attributes to for and while
You can attach a debugger attribute on a node.

```ocaml
for i = 0 to 10 do
  print_endline "I'm in a loop"
done
[@@log "The value of i is : %d", i]
```

And attributes could be chained

```ocaml
while true do
  print_endline "I'm in a while"
done
[@@log "Some log"][@@breakpoint]  
```
