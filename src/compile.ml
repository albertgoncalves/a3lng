open Ast
open Ir

type global =
  {
    mutable k : int;
    insts : inst Queue.t;
    labels : (string * string) Stack.t;
  }

let global =
  {
    k = 0;
    insts = Queue.create ();
    labels = Stack.create ();
  }

let next_k () =
  let ident = Printf.sprintf "__%d__" global.k in
  global.k <- global.k + 1;
  ident

let rec push_args args =
  args
  |> List.map expr_to_atom
  |> List.map (fun atom -> InstParam atom)
  |> List.iter (fun inst -> Queue.add inst global.insts)

and expr_to_atom =
  function
  | ExprInt n -> AtomInt n
  | ExprStr str -> AtomStr str
  | ExprIdent ident -> AtomIdent ident
  | ExprBinOp (op, l, r) ->
    (
      let ident = next_k () in
      Queue.add
        (InstLet (ident, BinOp (op, expr_to_atom l, expr_to_atom r)))
        global.insts;
      AtomIdent ident
    )
  | ExprCall (func, args) ->
    (
      match expr_to_atom func with
      | AtomIdent ident_func ->
        (
          let ident_result = next_k () in
          push_args args;
          Queue.add (InstCall (ident_func, Some ident_result)) global.insts;
          AtomIdent ident_result
        )
      | _ -> assert false
    )

let rec stmts_to_insts stmts =
  List.iter stmt_to_insts stmts

and stmt_to_insts =
  function
  | StmtVoid expr -> ignore (expr_to_atom expr)
  | StmtLet (ident, expr) ->
    Queue.add (InstLet (ident, Atom (expr_to_atom expr))) global.insts
  | StmtUpdate (target, ExprBinOp (op, l, r)) ->
    (
      match expr_to_atom target with
      | AtomIdent ident ->
        Queue.add
          (InstUpdate (ident, BinOp (op, expr_to_atom l, expr_to_atom r)))
          global.insts
      | _ -> assert false
    )
  | StmtUpdate (target, value) ->
    (
      match expr_to_atom target with
      | AtomIdent ident ->
        Queue.add (InstUpdate (ident, Atom (expr_to_atom value))) global.insts
      | _ -> assert false
    )
  | StmtLoop scope ->
    (
      let top = next_k () in
      let bottom = next_k () in
      Stack.push (top, bottom) global.labels;
      Queue.add (InstLabel (top, None)) global.insts;
      stmts_to_insts scope;
      Queue.add (InstGoto top) global.insts;
      ignore (Stack.pop global.labels);
      Queue.add (InstLabel (bottom, None)) global.insts;
    )
  | StmtIf (ExprBinOp (op, l, r), scope) ->
    (
      let label = next_k () in
      Queue.add
        (InstIfNot (BinOp (op, expr_to_atom l, expr_to_atom r), label))
        global.insts;
      stmts_to_insts scope;
      Queue.add (InstLabel (label, None)) global.insts;
    )
  | StmtIf (condition, scope) ->
    (
      let label = next_k () in
      Queue.add
        (InstIfNot (Atom (expr_to_atom condition), label))
        global.insts;
      stmts_to_insts scope;
      Queue.add (InstLabel (label, None)) global.insts;
    )
  | StmtBreak ->
    (
      let (_, bottom) = Stack.top global.labels in
      Queue.add (InstGoto bottom) global.insts;
    )
  | StmtReturn (Some expr) ->
    Queue.add (InstReturn (Some (expr_to_atom expr))) global.insts
  | StmtReturn None ->
    Queue.add (InstReturn None) global.insts

let func_to_insts (ident, args, scope) =
  assert (Queue.is_empty global.insts);
  assert (Stack.is_empty global.labels);
  Queue.add (InstLabel (ident, Some args)) global.insts;
  stmts_to_insts scope;
  assert (Stack.is_empty global.labels);
  let insts =
    global.insts
    |> Queue.to_seq
    |> List.of_seq in
  Queue.clear global.insts;
  insts
