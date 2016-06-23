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

(* Perform attributes substitution *)
let perform_attributes attr expr =
  (attr, expr)

(* Mapper for structure_item *)
let process_structure_item mapper item =
  match item.pstr_desc with
  | Pstr_attribute ({txt="debugger.reveal"; loc = location}, PStr []) ->
    Str.eval (Ppx.Fabric.reveal_loc location)
  | _ -> default_mapper.structure_item mapper item

(* Mapper for expression *)
let perform_expr expr =
  let new_attributes, new_expression =
    perform_attributes
      expr.pexp_attributes
      expr
  in {new_expression with pexp_attributes = new_attributes}

let process_expr mapper expr =
  if (Util.attributes_candidate expr.pexp_attributes)
  then expr
  else match expr.pexp_desc with
    | _ -> default_mapper.expr mapper expr

(* Mapper for structure *)
let process_structure = default_mapper.structure

(* Mapper for all transformation *)
let general_mapper = Ast_mapper.{
    default_mapper with
    structure = process_structure
  ; structure_item = process_structure_item
  ; expr = process_expr
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
