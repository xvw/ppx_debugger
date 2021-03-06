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

open Parsetree
open Asttypes


module Infix =
struct
  let (>|=) o f =
    match o with
    | Some x -> Some (f x)
    | None -> None

end


(* Open file as a list of string *)
let open_file filename =
  let channel = open_in filename in
  let rec aux acc =
    try let l = input_line channel in aux (l :: acc)
    with End_of_file -> List.rev acc
  in aux []

(* Convert timestamp to d-m-Y H:i*)
let to_date tm =
  let open Unix in
  let date = localtime tm in
  Printf.sprintf
    "%d-%d-%d %d:%d:%d"
    date.tm_mday
    date.tm_mon
    (date.tm_year + 1900)
    date.tm_hour
    date.tm_min
    date.tm_sec

let attributes_candidate attributes =
  List.exists
    (fun x ->
       let attr = (fst x).txt in
       List.mem attr DbgConfig.keywords
    )
    attributes
