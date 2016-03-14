[@@@breakpoint]
let () = for i = 0 to 10 do 
   (Printf.printf "Iteration n°%d\n" i) [@breakpoint i = 5]
done
[@@@log "Exit the loop"]
