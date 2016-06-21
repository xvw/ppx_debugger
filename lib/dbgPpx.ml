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

let raise_error ?(loc = !default_loc) message =
  let open Location in
  raise (Error (error ~loc message))

let define_loc ?(loc = !default_loc) value =
  { txt = value; loc = loc }

let loc = define_loc

let ident ?(loc = !default_loc) value =
  define_loc ~loc (Longident.Lident value)

let exp_ident x =
  Exp.ident (ident x)

let string value =
  Exp.constant (Const.string value)

let int value =
  Exp.constant (Const.int value)

let pattern name =
  Pat.var (loc name)

let binding name expr =
  Vb.mk (pattern name) expr

let import_function modname funcname =
  loc Longident.(Ldot (Lident modname, funcname))
  |> Exp.ident

let rec sequences_of = function
  | [] -> raise_error "Malformed sequence"
  | [x] -> x
  | x :: xs -> Exp.sequence x (sequences_of xs)


module Fabric =
struct

  let code_array =
    "debugger_module_code"

  let identity =
    let open Exp in
    fun_ Nolabel None (Pat.var (loc "x")) (exp_ident "x")

  let module_code file =
    let content = DbgUtil.open_file file in
    let lines = List.map string content in
    let arr = Exp.array lines in
    Str.value Nonrecursive [binding code_array arr]

  let print_endline ?(bs=true) value =
    let f =
      if bs
      then "print_endline"
      else "print_string" in
    let e = exp_ident f in
    Exp.apply e [Nolabel, string value]

  let press_enter ?(quiet=false) () =
    let f = import_function "Scanf" "scanf" in
    let open Exp in
    let a =
      apply f [
        Nolabel, string "%s\n"
      ; Nolabel, identity
      ]
    in
    let e = apply (exp_ident "ignore") [Nolabel, a] in
    if quiet
    then e
    else sequences_of [
        print_endline
          DbgColor.(scope [magenta ~bg:true (); white ()] "  Press [ENTER]  " )
      ; print_endline ""
      ; e
      ]

  let toplevel_press_enter ?(quiet=false) () =
    Str.eval (press_enter ~quiet ())

  let array_sub target i len =
    let f = import_function "Array" "sub" in
    Exp.(apply f [
        Nolabel, exp_ident code_array
      ; Nolabel, int i
      ; Nolabel, int (max 1 len)
      ])

  let array_iteri lambda arr =
    let f = import_function "Array" "iteri" in
    Exp.(apply f [
        Nolabel, lambda
      ; Nolabel, exp_ident code_array
      ])


  let reveal_loc loc =
    let open Location in
    let start  = loc.loc_start.Lexing.pos_lnum - 1 in
    let stop   = loc.loc_end.Lexing.pos_lnum - 1 in
    let length = stop - start in
    let arr_sub = array_sub code_array start length in ()


  let header txt file =
    let open DbgColor in
    let date = Unix.stat file in
    let hd =
      scope
        [green ~bg:true () ; black ()]
        ("   "^txt^"   ")
    in
    let md = Unix.(date.st_mtime) in
    let tl = scope [blue ~bg:true (); white ()] "  mtime  " in
    let tm =
      scope
        [blue (); white ~bg:true ()]
        (sprintf "  %s  " (DbgUtil.to_date md))
    in
    print_endline (hd ^ tl ^ tm)
    |> Str.eval

end
