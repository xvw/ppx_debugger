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

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location
open Lexing


let fail ?(loc = !default_loc) message =  
  let open Location in 
  raise (Error (error ~loc message))

(* Useful functions *)
module Helper =
struct

  let create_loc ?(loc = !default_loc) v = {txt = v; loc = loc}
  let create_ident ?(loc = !default_loc) v = create_loc ~loc (Lident v)
  let str_constant value = Exp.constant (Const_string (value, None))
  let int_constant value = Exp.constant (Const_int value)
  let pattern_var s = Pat.var (create_loc s)
  let pattern_any = Pat.any ()
  let exp_ident x = Exp.ident (create_ident x)
  
  let variable ?(recf=Nonrecursive) binding =
    Str.value
      recf
      (List.map
         (fun (name, expr) -> Vb.mk name expr) binding
      )

   let e_printf const location =
    let f = create_loc (Ldot (Lident "Printf", "sprintf")) in
    let file =  str_constant location.loc_start.pos_fname in
    let line = int_constant location.loc_start.pos_lnum in 
    let sprintf =
      Exp.(apply
             (ident f)
             [ "", const;
               "", file;
               "", line]
          )
    in Exp.(apply (ident (create_ident "print_endline")) ["", sprintf])
        
  let printf const location str =
    let f = create_loc (Ldot (Lident "Printf", "printf")) in
    let file =  str_constant location.loc_start.pos_fname in
    let line = int_constant location.loc_start.pos_lnum in 
    let e =
      Exp.(apply
             (ident f)
             [ "", const;
               "", file;
               "", line;
               "", str ]
          ) in
    Str.eval e

  let logf l format preapplied =
    let f = create_loc (Ldot (Lident "Printf", "sprintf")) in
    let e =
      Exp.(apply
             (ident f)
             (("", format) :: (List.map (fun x -> "", x) preapplied))
          )
    in printf (str_constant "<%s:%d> %s \n") l e

  let identity =
    Exp.fun_
      ""
      None
      (pattern_var "x")
      (Exp.ident (create_ident "x"))

  let wait_for_key =
    let f = create_loc (Ldot (Lident "Scanf", "scanf")) in
    Exp.(apply
           (ident f)
           [
             "", str_constant "%s\n";
             "", identity
           ]
        )
      
end


let create_breakpoint location =
  Helper.( variable [
      pattern_any,
      Exp.let_
        Nonrecursive 
        [Vb.mk pattern_any (
            e_printf (str_constant "<%s:%d> Breakpoint (press ENTER)") location
          )]
        wait_for_key
      ;
    ] )
  

let create_log location = function
  | PStr [str] ->
    begin
      match str.pstr_desc with
      | Pstr_eval (expr, _) ->
        begin match expr.pexp_desc with
          | Pexp_constant _ -> Helper.logf location expr []
          | Pexp_tuple (format :: arg) -> Helper.logf location format arg
          | _ -> fail "[@@@log ...] is malformed"
        end
      | _ -> fail "[@@@log ...] is malformed"
    end
  | _ -> fail "[@@@log ...] is malformed"
  


(* Replace all debugger attributes *)
let attr_replace s = 
  match s.pstr_desc with
  | Pstr_attribute (data, payload) ->
    begin
      match data.txt with
      | "log" -> create_log data.loc payload
      | "breakpoint" -> create_breakpoint data.loc
      | _ -> s
    end
  | _ -> s

let remap_ast _ s =
  List.map attr_replace s

(* Transform the syntax tree *)
let mapper argv =
  let super = default_mapper in {
    super with
    structure = remap_ast 
  }

(* Recording mapper *)
let _ =
  register "debugger_mapper" mapper
