let x = 10
[@@@breakpoint]
let _ = print_endline "Hello World"
[@@@log "This is a log %d %s %s", 10, "foo", "bar"]
let _ = print_endline "Foo bar"
let y = 11
[@@@breakpoint]
[@@@log "This is a log"]
let _ = print_endline "done"

