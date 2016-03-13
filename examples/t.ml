let x = 10
let _ =
  Printf.printf "\027[43m\027[34m LOG [sample.ml:2] \027[0m This is a log %d" 10;
  print_endline ""

let _ =
  Printf.printf
    "\n\027[44m BREAKPOINT [sample.ml:3] \027[0m\n\027[32m 1    let x = 10\n 2    [@@@log \"This is a log %d\", 10]\n\027[31m 3    [@@@breakpoint]\027[32m\n 4    let _ = print_endline \"Hello World\"\n 5    [@@@breakpoint]\027[0m\n";
  print_endline ""

let _ = print_endline "Hello World"
let _ =
  Printf.printf
    "\n\027[44m BREAKPOINT [sample.ml:5] \027[0m\n\027[32m 3    [@@@breakpoint]\n 4    let _ = print_endline \"Hello World\"\n\027[31m 5    [@@@breakpoint]\027[32m\n 6    let _ = print_endline \"Foo bar\"\n 7    let y = 11\027[0m\n";
  print_endline ""

let _ = print_endline "Foo bar"
let y = 11
let _ =
  Printf.printf
    "\n\027[44m BREAKPOINT [sample.ml:8] \027[0m\n\027[32m 6    let _ = print_endline \"Foo bar\"\n 7    let y = 11\n\027[31m 8    [@@@breakpoint]\027[32m\n 9    [@@@log \"This is a log:\"]\027[0m\n";
  print_endline ""
let _ =
  Printf.printf "\027[43m\027[34m LOG [sample.ml:9] \027[0m This is a log:";
  print_endline ""
