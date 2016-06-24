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
open Ast_mapper
open Ast_helper
open Asttypes

module Config = DbgConfig
module Color = DbgColor
module Ppx   = DbgPpx
module Util  = DbgUtil

open Util.Infix

(* Perform attributes substitution *)
let perform_attributes attributes expr =
  let open Ppx.Fabric in
  let rec aux (attr, e) = function
    | [] -> (List.rev attr, e)
    | ({txt = "debugger.reveal"; loc=loc}, PStr []) :: xs ->
      let point = let_in (reveal_loc expr.pexp_loc) e in
      aux (attr, point) xs
    | ({txt = "debugger.log"; loc=loc}, payload) :: xs ->
    let point = let_in (logf expr.pexp_loc payload) e in
    aux (attr, point) xs
    | x :: xs -> aux (x :: attr, e) xs
  in aux ([], expr) attributes

(* Mapper for structure_item *)
let process_structure_item mapper item =
  match item.pstr_desc with
  | Pstr_eval (e, attributes) ->
    Str.eval ~attrs:attributes (mapper.expr mapper e)
  | Pstr_attribute ({txt="debugger.reveal"; loc = location}, PStr []) ->
    Str.eval (Ppx.Fabric.reveal_loc location)
  | Pstr_attribute ({txt="debugger.log"; loc = location}, payload) ->
    Str.eval (Ppx.Fabric.logf location payload)
  | _ -> default_mapper.structure_item mapper item

(* Mapper for expression *)
let perform_expr mapper expr =
  let new_attributes, new_expression =
    perform_attributes
      expr.pexp_attributes
      expr
  in {new_expression with pexp_attributes = new_attributes}

(* Mapper for value binding *)
let perform_value_binding mapper vb =
  let new_attributes, new_expr =
    perform_attributes
      vb.pvb_attributes
      vb.pvb_expr
  in {vb with
      pvb_expr = (mapper.expr mapper new_expr)
    ; pvb_attributes = new_attributes
     }

(* Remap expression *)
let process_expr mapper expr =
  if Util.attributes_candidate expr.pexp_attributes
  then perform_expr mapper expr
  else match expr.pexp_desc with
    | Pexp_let (rf, vb, subexp) ->
      Exp.let_
        rf
        (List.map (mapper.value_binding mapper) vb)
        (mapper.expr mapper subexp)
    | Pexp_fun (a, e, pat, subexp) ->
      Exp.fun_ a e pat (mapper.expr mapper subexp)
    | Pexp_match (e, c) ->
      Exp.match_ (mapper.expr mapper e) c
    | Pexp_try (e, c) -> Exp.try_ (mapper.expr mapper e) c
    | Pexp_tuple xs -> Exp.tuple (List.map (fun x -> mapper.expr mapper x) xs)
    | Pexp_ifthenelse (i, th, e) ->
      Exp.ifthenelse i (mapper.expr mapper th) (e >|= mapper.expr mapper)
    | Pexp_for (p, e1, e2, dirf, e3) ->
      Exp.for_ p e1 e2 dirf (mapper.expr mapper e3)
    | Pexp_while (e1, e2) ->
      Exp.while_
        (mapper.expr mapper e1)
        (mapper.expr mapper e2)
    | Pexp_apply (e, ls) ->
      Exp.apply
        (mapper.expr mapper e)
        (List.map (fun (a, ex) -> a, (mapper.expr mapper ex)) ls)
    | _ -> default_mapper.expr mapper expr


(* Mapper for all transformation *)
let general_mapper = Ast_mapper.{
    default_mapper with
    structure_item = process_structure_item
  ; expr = process_expr
  ; value_binding = perform_value_binding
  }

let append_module_code _ = function
  | [] -> Ppx.raise_error "The module is empty"
  | (elt :: _) as str ->
    let open Location in
    let input = elt.pstr_loc.loc_start.Lexing.pos_fname in
    let open Ppx.Fabric in
    let pl = module_code input in
    pl
    :: active_stacktrace
    :: header "Start with ppx_debugger" input
    :: toplevel_press_enter ~quiet:false ()
    :: (general_mapper.structure general_mapper str)

(* New Mapper only for the toplevel *)
let toplevel_mapper =
  Ast_mapper.{
    default_mapper with
    structure   = append_module_code
    ; signature = fun _ -> default_mapper.signature default_mapper
  }
