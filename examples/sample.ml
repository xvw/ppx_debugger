[@@@breakpoint]
let x = 10 [@breakpoint]
[@@@log "This is a log %d", 10]
[@@@breakpoint x = 11 || true]
let _ = print_endline "Hello World"
let _ = print_endline "Foo bar"
let y = 11 [@@breakpoint]
[@@@log "An exception will be apparead soon..."]
let x = List.hd [] [@@catch 17]

[@@@log "This is a log:"]
let _ = print_endline "done"
let f =
  let _ =
    let r = Printexc.get_callstack 10 in
    print_endline (r|>Printexc.raw_backtrace_to_string)
  in 
  let _ = print_endline "after" in
  let y = 12 in
  let _ =
    for i = 0 to 10 do
      let _ = (List.hd [])[@catch "a"] in
      (print_endline "yolo") [@log "salut:%d", i][@breakpoint i = 7]
    done
  in x + y [@@breakpoint]

type a
type b
type c
type d

[@@@breakpoint]


