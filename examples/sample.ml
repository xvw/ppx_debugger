let x = 10
let y = 11
[@@@log "x : %d and y : %d", x, y]

let _ = print_endline "Hello World"
[@@@breakpoint]

let _ =
  for i = 0 to 10 do
    (print_endline "Hello")
      [@log "Value of i : %d", i]
      (* A breakpoint if i = 5*)
      [@breakpoint i = 5]
  done

[@@@log "The loop is finished"]

(* This code should be crashed but ... *)
let head = List.hd [] [@@catch "bar"]
[@@@log "Head is has this value : %s", head]

(* this code crash *)
let head2 = List.hd [] [@@catch]


