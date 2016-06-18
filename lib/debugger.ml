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

(* Default mapper to specialize behaviour of mapper *)
module Polyfill =
struct

  let structure_item mapper item =
    default_mapper.structure_item mapper item

  let payload_mapper mapper = function
    | PStr str -> PStr (default_mapper.structure mapper str)
    | elt -> elt

  let module_expr_mapper mapper modl =
    match modl.pmod_desc with
    | Pmod_structure str ->
      let strct = default_mapper.structure mapper str in {
        modl with pmod_desc = (Pmod_structure strct)
      }
    | elt -> modl

end

(* Mapper for structures *)
let general_structure mapper str =
  let r =
    List.map
      (fun x -> Polyfill.structure_item mapper x)
      str in
  let pl = Ast_helper.Str.eval (Ppx.Fabric.print_endline "Hello World") in
  pl :: r

(* New Mapper *)
let new_mapper =
  Ast_mapper.{
    default_mapper with
    payload     = Polyfill.payload_mapper
  ; module_expr = Polyfill.module_expr_mapper
  ; structure   = general_structure
  }
