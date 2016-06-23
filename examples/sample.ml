
let x = "hello"

let test x =
  let succx = succ x in
  let succxx = succ succx in
  succxx [@@debugger.reveal]


[@@@debugger.reveal]
module X = struct
  let y = 10000
  let y = 2000
end [@@debugger.reveal]

let test x =
  let succx = succ x in
  let succxx = succ succx in
  succxx
  [@@debugger.reveal]
