open Ast
open Printf

let program, run =
  let filebuf = Lexing.from_channel stdin in
  let get_offset () =
    let pos = Lexing.lexeme_start_p filebuf in
    let open Lexing in
    let column = pos.pos_cnum - pos.pos_bol in
    string_of_int pos.pos_lnum ^ ":" ^ string_of_int column
  in
  try Parser.prog Lexer.main filebuf with
  | Lexer.Error | Parser.Error ->
      printf "Error at line %s\n" (get_offset ());
      exit 1

open Llvm

[@@@ocaml.warning "-3"] (* TODO: Remove this when LLVM 16 is available *)

let c = global_context ()
let () = set_opaque_pointers c false
let ty = i64_type c

let ( |> ) x f = f x

type state =
    { fn: llvalue;
      blk: llbasicblock;
      vars: (string * llvalue) list }

let bb state = builder_at_end c state.blk
let new_block state name = append_block c name state.fn
let find state v =
  try List.assoc v state.vars with Not_found ->
    eprintf "Unknown variable %s\n" v;
    raise Not_found
let cont (v, state) dest_blk =
  build_br dest_blk (bb state) |> ignore;
  v, state

let rec expr state = function
  | Int n -> const_int ty n, state
  | Var x -> find state x, state
  | BinOp(op, f, g) ->
      let f, state = expr state f in
      let g, state = expr state g in
      let build, name = match op with
        | `Add -> build_add, "add"
        | `Sub -> build_sub, "sub"
        | `Leq -> build_icmp Icmp.Sle, "leq" in
      build f g name (bb state), state
  | If(p, t, f) ->
      let t_blk = new_block state "pass" in
      let f_blk = new_block state "fail" in
      let k_blk = new_block state "cont" in
      let cond, state = expr state p in
      build_cond_br cond t_blk f_blk (bb state) |> ignore;
      let t, state = cont (expr { state with blk = t_blk } t) k_blk in
      let f, state = cont (expr { state with blk = f_blk } f) k_blk in
      build_phi [t, t_blk; f, f_blk] "join" (bb state), state
  | Apply(f, arg) ->
      let f, state = expr state f in
      let arg, state = expr state arg in
      build_call f [|arg|] "apply" (bb state), state

let defn m vars = function
  | LetRec(f, arg, body) ->
      let ty = function_type ty [| ty |] in
      let fn = define_function f ty m in
      let vars' = (arg, param fn 0) :: (f, fn) :: vars in
      let body, state =
        expr { fn = fn; blk = entry_block fn; vars = vars' } body in
      build_ret body (bb state) |> ignore;
      (f, fn) :: vars

let int n = const_int ty n

let main filename =
  let m = create_module c filename in

  let string = pointer_type (i8_type c) in

  let print =
    declare_function "printf" (var_arg_function_type ty [|string|]) m in

  let main = define_function "main" (function_type ty [| |]) m in
  let blk = entry_block main in
  let bb = builder_at_end c blk in

  let str s = define_global "buf" (const_stringz c s) m in
  let int_spec = build_gep (str "%d\n") [| int 0; int 0 |] "int_spec" bb in

  let vars = List.fold_left (defn m) [] program in
  let n, _ = expr { fn = main; blk = blk; vars = vars } run in

  build_call print [| int_spec; n |] "" bb |> ignore;

  build_ret (int 0) bb |> ignore;

  if not (Llvm_bitwriter.write_bitcode_file m filename) then exit 1;
  dispose_module m

let () = match Sys.argv with
  | [|_; filename|] -> main filename
  | _ as a -> Printf.eprintf "Usage: %s <file>\n" a.(0)
