> This project is currently rewritted for Caml 4.03 ! 

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


## Installation 

`opam install ppx_debugger`

or, without OPAM :

```
git clone https://github.com/xvw/ppx_debugger.git
./configure
make 
make install
```

## Usage

You just have to pass "-ppx ppx_debugger" and use "-g" for launch OCaml
in debug-mode


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

Attributes could be chained : `[@@attr1][@@attr2]...`

### log 

-  `[@log format, arg1, arg2...etc]`
-  `[@@log format, arg1, arg2...etc]`
-  `[@@@log format, arg1, arg2...etc]`

Log write on stdout a message with a location. The first parameter of a log is a format so you 
can print some fixed values (separated by coma).  
For example : 

```ocaml
[@@@log "Enter in a Loop"]
let () = for i = 0 to 10 do 
   (print_endline "Yo") [@log "value of %s : %d", "i", i]
done
[@@@log "Exit the loop"]
```
![Sample of log](http://full.ouplo.com/10/b/It2t.gif)

### breakpoint 

Breakpoint interrupt the execution. A breakpoint could be decorated with a predicate. For 
example, this code interrupt at the begining of the execution, and once during the execution of
the loop :

- `[@breakpoint]`
- `[@@breakpoint]`
- `[@@@breakpoint]`
- `[@breakpoint boolean_expression]`
- `[@@breakpoint boolean_expression]`
- `[@@@breakpoint boolean_expression]`

```ocaml
[@@@breakpoint]
let () = for i = 0 to 10 do 
   (Printf.printf "Iteration n°%d\n" i) [@breakpoint i = 5]
done
[@@@log "Exit the loop"]
```
![Sample of breakpoint](http://full.ouplo.com/10/b/TDH7.gif)


### catch

Catch try an expression and catch his failure. Catch could has a "substitute" value. For 
example : 

```ocaml
let () = for i = 0 to 10 do
   let x = (10 / i) [@catch 20] in
   (Printf.printf "Iteration n°%d, value of x :%d\n" i x)
  done

[@@@log "Exit the loop, try to get a List head on an empty list"]
let head = List.hd [] [@@catch] (*Here we let it crash !*)
```
![Sample of crash](http://full.ouplo.com/10/b/A8Te.gif)


