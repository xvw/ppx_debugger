(* A small preprocessor for interactive debugging
 *
 * Copyright (c) 2015 Xavier Van de Woestyne <xaviervdw@gmail.com>
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
*)

open Printf

(* Create a color on background or foreground *)
let make ?(bg=false) =
  let f = if bg then 4 else 3 in
  sprintf "\027[%d%dm" f

(* Presaved colors *)
let reset = "\027[0m"
let black   ?(bg=false) ()  = make ~bg 0
let red     ?(bg=false) ()  = make ~bg 1
let green   ?(bg=false) ()  = make ~bg 2
let yellow  ?(bg=false) ()  = make ~bg 3
let blue    ?(bg=false) ()  = make ~bg 4
let magenta ?(bg=false) ()  = make ~bg 5
let cyan    ?(bg=false) ()  = make ~bg 6
let white   ?(bg=false) ()  = make ~bg 7

let scope l text =
  let li = List.fold_left (^) "" l in
  sprintf "%s%s%s" li text reset
