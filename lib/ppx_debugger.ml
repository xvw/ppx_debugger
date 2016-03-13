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

let (>|=) o f =
  match o with
  | Some x -> Some (f x)
  | None -> None

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

  let int_constant value =
    Exp.constant (Const_int value)

  let _true = Exp.construct (ident "true") None
  let _false = Exp.construct (ident "false") None
  let _unit = Exp.construct (ident "()") None

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


  let let_in expr1 expr2 =
    let open Exp in
    let_ Nonrecursive [Vb.mk (Pat.any ()) expr1] expr2

  let let_lid_in name expr1 expr2 =
    let open Exp in
    let_ Nonrecursive [Vb.mk (Pat.var (loc name)) expr1] expr2

  let identity =
    let open Exp in
    fun_ ""
      None (Pat.var (loc "x"))
      (x_ident "x")

  let active_stacktrace =
    let c = func_apply "Printexc" "record_backtrace" in
    Exp.apply c ["", _true]

  let print_endln v =
    let open Exp in
    apply (x_ident "print_endline") ["", string_constant v]

  let print_endl = print_endln ""

  let print_exp_as_str str =
    let open Exp in
    apply (x_ident "print_string") ["", str]

  let print_str str =
    str |> string_constant |> print_exp_as_str

  let raw_wait_input =
    let f = func_apply "Scanf" "scanf" in
    let open Exp in
    let a =
      apply f [
        "", string_constant "%s\n"
      ; "", identity
      ] in
    apply (x_ident "ignore") ["", a]

  let wait_input =
    let_in (print_endln "<wait-for-enter>") raw_wait_input

  let rec sequences_of = function
    | [] -> raise_error "Malformed sequence"
    | [x] -> x
    | x :: xs -> Exp.sequence x (sequences_of xs)


  let fragment_file location border_top line file =
    let ctn = file.content in
    let fnd = location.Location.loc_end.Lexing.pos_lnum in
    let bmin, bmax = 0, (Array.length ctn) - 1 in
    let i = max (line - border_top - 1) bmin in
    let j = min (fnd + border_top + 1) bmax in
    let len = j - i in
    (i, Array.sub ctn i len)

  let formatting_substitution location str =
    let mess = "CHANGE VALUE FOR AVOIDING EXCEPTION" in
    let fname, li, _, _ = file_data location in
    "\n"
    ^ sprintf "%s %s [%s:%d] %s\n" (Color.blue ~bg:true ()) mess fname li Color.reset
    ^ Color.green ()
    ^ str
    ^ Color.reset
    ^ "\n"

  let formating_fragment location str =
    let fname, li, _, _ = file_data location in
    "\n"
    ^ sprintf "%s BREAKPOINT [%s:%d] %s\n" (Color.blue ~bg:true ()) fname li Color.reset
    ^ Color.green ()
    ^ str
    ^ Color.reset
    ^ "\n"

  let raw_format_fragment f location l arr =
    Array.mapi (fun i x ->
        let open Location in
        let line = (l + i + 1) in
        if line >= location.loc_start.Lexing.pos_lnum
           && line <= location.loc_end.Lexing.pos_lnum
        then
          sprintf
            "%s%- 5d %s%s"
            (Color.yellow ())
            line
            x
            (Color.green ())
        else sprintf "%- 5d %s" line x
      ) arr
    |> array_join "\n"
    |> f location
    |> string_constant

  let format_fragment = raw_format_fragment formating_fragment


  let print_fragment location btop line file =
    let i, s = fragment_file location btop line file in
    let str = format_fragment location i s in
    print_exp_as_str str

  let print_vb_sub location btop line file =
    let i, s = fragment_file location btop line file in
    let str = raw_format_fragment formatting_substitution location i s in
    print_exp_as_str str

  let fragment_wit_input location b l file =
    sequences_of [
      print_fragment location b l file
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

  let expr_candidate attrs =
    List.exists (function
        | ({txt = "breakpoint"; loc = _}, _) -> true
        | ({txt = "catch"; loc = _}, _) -> true
        | _ -> false
      ) attrs

  let get_backtrace =
    Exp.apply
      (func_apply "Printexc" "get_backtrace")
      ["", _unit]

  let exit_with id =
    Exp.apply (x_ident "exit") ["", int_constant id]

  let try_catch_one exprSucc exprFail  =
    Exp.try_ exprSucc [Exp.case (Pat.var (loc "expt")) exprFail]

  let exception_s name =
    let f = func_apply "Printexc" "to_string" in
    Exp.apply f ["", x_ident name]


  let print_backtrace location result =
    let fname, line, _, _ = file_data location in
    sequences_of [
      pprintf "printf"
        (sprintf "\n%s %s [%s:%d] %s%s\n"
           (Color.red ~bg:true ())
           "%s"
           fname
           line
           Color.reset
           (Color.red ())
         |> string_constant
        ) [exception_s "expt"]
    ; print_exp_as_str (x_ident "backtrace")
    ; print_str (Color.reset)
    ; result
    ]

  let catch_stack locat e result =
    let e1 = let_lid_in "backtrace" get_backtrace (print_backtrace locat result) in
    let_in active_stacktrace (try_catch_one e e1)

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

let perform_vb_sub location file line value =
  let open Tools in
  match value.pstr_desc with
  | Pstr_eval (e, _) ->
    Tools.(
      let_in (
        sequences_of [
          (print_vb_sub location 3 line file);
          wait_input
        ]
      ) e)
  | _ -> Tools.raise_error "Malformed catch"


let perform_attributes locat line file attrib expr =
  let rec aux (attr, e) = function
    | [] -> (List.rev attr, e)
    | ({txt = "breakpoint"; loc=loc}, PStr []) :: xs ->
      aux (attr, Tools.(let_in (fragment_wit_input locat 3 line file) e)) xs
    | ({txt = "breakpoint"; loc=loc}, PStr [strct]) :: xs ->
      aux (attr, Tools.(let_in (fragment_with_predicat locat 3 line file strct) e)) xs
    | ({txt = "catch"; loc=loc}, PStr []) :: xs ->
      aux (attr, Tools.(catch_stack locat e (exit_with 1))) xs
    | ({txt = "catch"; loc=loc}, PStr [value]) :: xs ->
      let sub = perform_vb_sub locat file line value in
      aux (attr, Tools.(catch_stack locat e sub)) xs
    | ({txt = "log"; loc=loc}, PStr [value]) :: xs ->
      aux (attr, Tools.(logf locat value)) xs
    | x :: xs -> aux (x :: attr, e) xs
  in aux ([], expr) attrib

let perform_expr mapper exp =
  let _ = print_endline "test" in
  let locat = exp.pexp_loc in
  let fname, line, _, _ = Tools.file_data locat in
  let file = Tools.open_module fname in
  let new_attr, new_exp =
    perform_attributes
      locat
      line
      file
      exp.pexp_attributes
      exp
  in { new_exp with pexp_attributes = new_attr }

let perform_vb mapper vb =
  let open Ast_mapper in
  let locat = vb.pvb_loc in
  let fname, line, _, _ = Tools.file_data locat in
  let file = Tools.open_module fname in
  let new_attr, expr =
    perform_attributes
      locat
      line
      file
      vb.pvb_attributes
      vb.pvb_expr
  in { vb with
       pvb_expr = (mapper.expr mapper expr)
     ; pvb_attributes = new_attr
     }


let process_value_binding mapper vb =
  if Tools.expr_candidate vb.pvb_attributes
  then perform_vb mapper vb
  else Ast_mapper.(default_mapper.value_binding mapper vb)

let process_expression mapper exp =
  if Tools.expr_candidate exp.pexp_attributes
  then perform_expr mapper exp
  else
    let open Ast_mapper in
    match exp.pexp_desc with
    | Pexp_let (rf, vb, sub_expr) ->
      Exp.let_ rf
        (List.map (fun x -> mapper.value_binding mapper x) vb)
        (mapper.expr mapper sub_expr)
    | Pexp_fun (a, e, pat, sub_expr) -> Exp.fun_ a e pat (mapper.expr mapper sub_expr)
    | Pexp_match (e, c) -> Exp.match_ (mapper.expr mapper e) c
    | Pexp_try (e, c) -> Exp.try_ (mapper.expr mapper e) c
    | Pexp_tuple xs -> Exp.tuple (List.map (fun x -> mapper.expr mapper x) xs)
    | Pexp_ifthenelse (i, th, e) ->
      Exp.ifthenelse i (mapper.expr mapper th) (e >|= mapper.expr mapper)
    | Pexp_for (p, e1, e2, dirf, e3) ->
      Exp.for_ p e1 e2 dirf (mapper.expr mapper e3)
    | _ -> default_mapper.expr mapper exp


let debug_mapper =
  Ast_mapper.{
    default_mapper with
    structure_item = process_item
  ; value_binding = process_value_binding
  ; expr = process_expression
  }

let () =
  Ast_mapper.run_main (fun argv -> debug_mapper)
