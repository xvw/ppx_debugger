> Ppx_debugger is still in developement (but is usable)

# Ppx_debugger

> A small semi-interactive debugger for OCaml using ppx.

## Summary 

Ppx_debugger provide some extension for display some information about the execution of a
program. Your code source must be extended with attributes to provide a compiled-debug output.
For example : 

```ocaml
let x = 10
let y = 11
[@@@log "x : %d and y : %d", x, y]

let _ = print_endline "Hello World"
[@@@breakpoint]

let _ =
  for i = 0 to 10 do
    (print_endline "Hello")
      [@log "Value of i : %d", i]
      (* A breakpoint if i = 5*)
      [@breakpoint i = 5]
  done

[@@@log "The loop is finished"]

(* This code should be crashed but ... *)
let head = List.hd [] [@@catch "bar"]
[@@@log "Head is has this value : %s", head]

(* this code crash *)
let head2 = List.hd [] [@@catch]

```

Compiled with : `ocamlc -ppx ppx_debugger.native -g sample.ml` provide this execution 

![sample1](http://full.ouplo.com/10/a/zWI6.gif)

## Extension 

Each extension attribute could be used with @, @@, or @@@ (according to the OCaml manual) :

```ocaml
let x = 1 

[@@@attribute parameters] (* Block attribute *)

let f = 
  let x = 10 
  let y = 11 
  in x + y 
  [@@attribute parameters] (* Attribute linked to "f" *)
  
let _ = 
    for i = 0 to 10 do 
        (print_endline "foo") [@attribute parameters] (* Attribute linked to  
            print_endline "foo"
        *)
    done
```

Attributes could be chained : `[@@attr1][@attr2]...`

### log 



### breakpoint 

### catch
