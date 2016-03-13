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


module Color =
struct

  let mk ?(bg=false) =
    let f = if bg then 4 else 3 in
    sprintf "\027[%d%dm" f

  let reset                   = "\027[0m"
  let red     ?(bg=false) ()  = mk ~bg 1
  let green   ?(bg=false) ()  = mk ~bg 2
  let yellow  ?(bg=false) ()  = mk ~bg 3
  let blue    ?(bg=false) ()  = mk ~bg 4
  let magenta ?(bg=false) ()  = mk ~bg 5
  let cyan    ?(bg=false) ()  = mk ~bg 6
  let white   ?(bg=false) ()  = mk ~bg 7

end

module Tools =
struct


  let c_loc ?(loc = !default_loc) value = {
    txt = value
    ; loc = loc
  }
  let loc = c_loc

  let ident ?(loc = !default_loc) value =
    c_loc ~loc (Longident.Lident value)

  let x_ident x = Exp.ident (ident x)

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


  type active_file = {
    name    : string
  ; content : string array
  }

  let func_apply modul func =
    loc Longident.(Ldot (Lident modul, func))
    |> Exp.ident

  let string_constant value =
    Exp.constant (Const_string (value, None))

  let pprintf func format params =
    let parameters_expanded = List.map (fun x -> "", x) params in
    let open Exp in
    apply
      (func_apply "Printf" func)
       (("", format) :: parameters_expanded)

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

  let fragment_file border_top line file =
    let ctn = file.content in
    let bmin, bmax = 0, (Array.length ctn) - 1 in
    let i = max (line - border_top) bmin in
    let j = min (line + border_top) bmax in
    let len = j - (i+1) in
    (i, Array.sub ctn i len)

  let formating_fragment location str =
    let fname, li, _, _ = file_data location in
    "\n"
    ^ sprintf "%s BREAKPOINT [%s:%d] %s\n" (Color.blue ~bg:true ()) fname li Color.reset
    ^ Color.green ()
    ^ str
    ^ Color.reset
    ^ "\n"

  let format_fragment location l arr =
    Array.mapi (fun i x ->
        let open Location in
        let line = (l + i + 1) in
        if line = location.loc_start.Lexing.pos_lnum then
          sprintf
            "%s%- 5d %s%s"
            (Color.red ())
            line
            x
            (Color.green ())
        else sprintf "%- 5d %s" line x
      ) arr
    |> array_join "\n"
    |> formating_fragment location
    |> string_constant


  let identity =
    let open Exp in
    fun_ ""
      None (Pat.var (loc "x"))
      (x_ident "x")

  let print_endl =
    let open Exp in
    apply (x_ident "print_endline") ["", string_constant ""]

  let print_exp_as_str str =
    let open Exp in
    apply (x_ident "print_string") ["", str]

  let wait_input =
    let f = func_apply "Scanf" "scanf" in
    let open Exp in
    let a =
      apply f [
        "", string_constant "%s\n"
      ; "", identity
      ] in
    apply (x_ident "ignore") ["", a]

  let rec sequences_of = function
    | [] -> raise_error "Malformed sequence"
    | [x] -> x
    | x :: xs -> Exp.sequence x (sequences_of xs)

  let print_fragment location btop line file =
    let i, s = fragment_file btop line file in
    let str = format_fragment location i s in
    print_exp_as_str str

  let fragment_wit_input location b l file =
    sequences_of [
      print_fragment location b l file
    ; print_endl
    ; wait_input
    ]

  let fragment_with_predicat location b l file strct =
    match strct.pstr_desc with
    | Pstr_eval (predicate, _) ->
      Exp.ifthenelse predicate (fragment_wit_input location b l file) None
    | _ -> raise_error "Malformed breakpoint"

  let format_log location s =
    let fname, line, _, _ = file_data location in
    sprintf "%s%s LOG [%s:%d] %s %s"
      (Color.yellow ~bg:true ())
      (Color.blue ())
      fname
      line
      Color.reset
      s
    |> string_constant

  let format_log_e location = function
    | Pexp_constant (Const_string (s, _)) -> format_log location s
    | _ -> raise_error "Log format need to be a string"

  let logf location structure_item =
    let f = pprintf "printf" in
    match structure_item.pstr_desc with
    | Pstr_eval (expr, _) -> begin
        match expr.pexp_desc with
        | Pexp_constant (Const_string (s, _)) ->
          sequences_of [f (format_log location s) []; print_endl]
        | Pexp_tuple (e :: arg) ->
          sequences_of [f (format_log_e location e.pexp_desc) arg; print_endl]
        | _ -> raise_error "Malformed log"
      end
    | _ -> raise_error "Malformed log"

  let open_module filename = {
      name    = filename
    ; content = open_file filename
    }

end

 let perform_float_attributes mapper item = function
  | ({txt="breakpoint"; loc = location}, PStr []) ->
    let fname, line, bol, c = Tools.file_data location in
    let file = Tools.open_module fname in
    Str.eval (Tools.fragment_wit_input location 3 line file)
  | ({txt="breakpoint"; loc = location}, PStr [expr]) ->
    let fname, line, bol, c = Tools.file_data location in
    let file = Tools.open_module fname in
    Str.eval (Tools.fragment_with_predicat location 3 line file expr)
  | ({txt="log"; loc = location}, PStr [str]) ->
    Str.eval (Tools.logf location str)
  | _ ->  Ast_mapper.(default_mapper.structure_item mapper item)

let process_item mapper item =
  match item.pstr_desc with
  | Pstr_attribute attr -> perform_float_attributes mapper item attr
  | _ -> Ast_mapper.(default_mapper.structure_item mapper item)


let debug_mapper =
  Ast_mapper.{
    default_mapper with
    structure_item = process_item
  }

let () =
  Ast_mapper.run_main (fun argv -> debug_mapper)
