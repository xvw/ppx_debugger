
let x = "hello" [@@debugger.log "Hello World"][@@kep]


let _ = for i = 0 to 15 do
    (print_endline "an interation") [@debugger.log "I = %d", i]
  done
