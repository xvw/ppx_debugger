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

module Color = DbgColor
module Ppx   = DbgPpx
module Util  = DbgUtil

(* Mapper for structure_item *)
let structure_item = default_mapper.structure_item
let structure = default_mapper.structure

(* Mapper for all transformation *)
let general_mapper = Ast_mapper.{
    default_mapper with structure = structure
  }

let append_module_code _ = function
  | [] -> Ppx.raise_error "The module is empty"
  | (elt :: _) as str ->
    let open Location in
    let input = elt.pstr_loc.loc_start.Lexing.pos_fname in
    let pl = Ppx.Fabric.module_code input in
    pl :: (general_mapper.structure general_mapper str)

(* New Mapper only for the toplevel *)
let toplevel_mapper =
  Ast_mapper.{
    default_mapper with
    structure   = append_module_code
    ; signature = fun _ -> default_mapper.signature default_mapper
  }
