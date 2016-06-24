
let x = "hello" [@@debugger.reveal]

let test x =
  let succx = succ x [@@debugger.reveal] in
  let succxx = succ succx [@debugger.reveal] in
  succxx
[@@debugger.reveal]


[@@@debugger.reveal]


module X = struct
  let y = 10000 [@debugger.reveal]
  let y = 2000
end

let test x =
  let succx = succ x in
  let succxx = succ succx in
  succxx
  [@@debugger.reveal]

[@@@debugger.reveal]
