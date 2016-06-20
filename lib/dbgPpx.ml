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

module Fabric =
struct


  let module_code file =
    let content = DbgUtil.open_file file in
    let lines = List.map string content in
    let arr = Exp.array lines in
    Str.value Nonrecursive [binding "debugger_module_code" arr]


  let print_endline value =
    let e = exp_ident "print_endline" in
    Exp.apply e [Nolabel, string value]

  let header txt =
    let open DbgColor in
    let str = scope [green ~bg:true () ; black ()] ("   "^txt^"   ") in
    print_endline str
    |> Str.eval

end
