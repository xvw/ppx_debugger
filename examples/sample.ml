let x = 10
[@@@log "This is a log %d", 10]
[@@@breakpoint x = 11 || true]
let _ = print_endline "Hello World"
[@@@breakpoint]
let _ = print_endline "Foo bar"
let y = 11
[@@@log "An exception will be apparead soon..."]
let x = List.hd [] [@@catch 17]
[@@@log "x:%d", x]
let r = List.hd [] [@@catch]
[@@@breakpoint]
[@@@log "This is a log:"]
let _ = print_endline "done"
let f x = x [@@trace]
