let x = 10
[@@@log "This is a log %d", 10]
[@@@breakpoint]
let _ = print_endline "Hello World"
[@@@breakpoint]
let _ = print_endline "Foo bar"
let y = 11
[@@@breakpoint]
[@@@log "This is a log:"]
let _ = print_endline "done"

