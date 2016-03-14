[@@@log "Enter in a Loop"]
let () = for i = 0 to 10 do 
   (print_endline "Yo") [@log "value of %s : %d", "i", i]
done
[@@@log "Exit the loop"]
