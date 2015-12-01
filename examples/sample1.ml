(* a Simple example *)



let a = "Test"
[@@@log "value A is initialized"]

let b = 128
[@@@log "value B is initialized with %d and %s", b, a]


let _ =
  for i = 0 to 10 do
    print_endline "in for"
  done
  [@@log "Value of i : %d", i][@@breakpoint]


[@@@breakpoint]
let _ = print_endline "Hello World"
