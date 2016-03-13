let x = 10
[@@@log "This is a log %d", 10]
[@@@breakpoint x = 11 or true]
let _ = print_endline "Hello World"
[@@@breakpoint]
let _ = print_endline "Foo bar"
let y = 11
let r = List.hd [] [@@catch]
[@@@breakpoint]
[@@@log "This is a log:"]
let _ = print_endline "done"

let f x = x [@@trace]

let g x =
  let a = 10 [@trace] in
  let b = 11 [@trace] in
  (a, b)
  [@@trace]
