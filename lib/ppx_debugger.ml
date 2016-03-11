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
open Parsetree
open Asttypes
open Ast_helper

module Tools =
struct

  type active_file = {
    filename    : string
  ; filecontent : string array
  ; linenum     : int
  ; cnum        : int
  ; bol         : int
  }

  let open_file filename =
    let channel = open_in filename in
    let rec aux i acc =
      try
        let l = input_line channel in
        aux (succ i) (l :: acc)
      with End_of_file ->
        let result = Array.make i "" in
        let _  = List.fold_left (fun i elt ->
            let _ = result.(i) <- elt in pred i
          ) (pred i) acc
        in result
    in aux 0 []

  let array_reduce f array =
    let len = Array.length array in
    if len = 0 then
      raise (Failure "Empty Array")
    else
      let acc = ref array.(0) in
      let _ =
        for i = 1 to len -1 do
          acc := f !acc array.(i)
        done in !acc

  let array_join sep =
    array_reduce (fun a b -> a ^ sep ^ b)


  let c_loc ?(loc = !default_loc) value = {
    txt = value
    ; loc = loc
  }
  let loc = c_loc

  let ident ?(loc = !default_loc) value =
    c_loc ~loc (Longident.Lident value)

  let raise_error ?(loc = !default_loc) message =
    let open Location in
    raise (Error (error ~loc message))

  let file_data location =
    let open Location in
    (
      location.loc_start.Lexing.pos_fname
    , location.loc_start.Lexing.pos_lnum
    , location.loc_start.Lexing.pos_bol
    , location.loc_start.Lexing.pos_cnum
    )

  let open_location location =
    let (file, line, bol, c) = file_data location in {
      filename    = file
    ; filecontent = open_file file
    ; linenum     = line
    ; cnum        = c
    ; bol         = bol
    }

end
