
let x = "hello"
let () = print_endline "hello"
[@@@debugger.reveal]
module X = struct
  let y = 10000
  let y = 2000
end [@@debugger.reveal]
