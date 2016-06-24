
let x = "hello" [@@debugger.reveal]

[@@@debugger.log
  "Value of x:%d %s",
  x,
  "toto"
]

let test x =
  x + 234
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
