[@@@log "yo"]
let _ =
  for i = 0 to 10 do
    (print_endline "Yo")
    [@log "yo"]
  done
