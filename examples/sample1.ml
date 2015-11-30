(* a Simple example *)



let a = "Test"
[@@@log "value A is initialized"]

let b = 128
[@@@logf "value B is initialized with %d and %s", b, a]
