<<<<<<< HEAD
let () = for i = 0 to 10 do
   let x = (10 / i) [@catch 20] in
   (Printf.printf "Iteration n°%d, value of x :%d\n" i x)
  done

[@@@log "Exit the loop, try to get a List head on an empty list"]
let head = List.hd [] [@@catch] (*Here we let it crash !*)
=======
let () = for i = 1 to 10 do
    let x = (10 / i) [@catch 20] in
    (Printf.printf "Iteration n°%d, value of x :%d\n" i x)
  done

module X = struct
  let y = 10000
end


let () = print_endline "fin"
>>>>>>> new-version
