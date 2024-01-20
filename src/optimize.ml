open Ast
open Ir

module StringSet = Set.Make (String)

type global =
  {
    mutable lets : StringSet.t;
    mutable updates : StringSet.t;
    mutable reads : StringSet.t;
  }

let global =
  {
    lets = StringSet.empty;
    updates = StringSet.empty;
    reads = StringSet.empty;
  }

let reset () =
  global.lets <- StringSet.empty;
  global.updates <- StringSet.empty;
  global.reads <- StringSet.empty

let rec walk_insts =
  function
  | [] -> ()
  | inst :: insts ->
    (
      (
        match inst with
        | InstLabel _ | InstGoto _ | InstCall (_, None) -> ()
        | InstLet (ident, atom_op) ->
          (
            walk_atom_op atom_op;
            global.lets <- StringSet.add ident global.lets
          )
        | InstUpdate (ident, atom_op) ->
          (
            walk_atom_op atom_op;
            global.updates <- StringSet.add ident global.updates
          )
        | InstCall (_, Some ident) ->
          global.lets <- StringSet.add ident global.lets
        | InstParam atom -> walk_atom atom
        | InstIfNot (atom_op, _) -> walk_atom_op atom_op
        | InstReturn (Some atom) -> walk_atom atom
        | InstReturn None -> ()
      );
      walk_insts insts
    )

and walk_atom_op =
  function
  | Atom atom -> walk_atom atom
  | BinOp (_, l, r) -> List.iter walk_atom [l; r]

and walk_atom =
  function
  | AtomInt _ | AtomStr _ -> ()
  | AtomIdent ident -> global.reads <- StringSet.add ident global.reads

let remove_unused unused =
  function
  | InstCall (label, Some ident) when StringSet.mem ident unused ->
    Some (InstCall (label, None))
  | InstLet (ident, _)
  | InstUpdate (ident, _) when StringSet.mem ident unused -> None
  | inst -> Some inst

let eval_op op l r =
  match op with
  | OpAdd -> Some (AtomInt (l + r))
  | OpMul -> Some (AtomInt (l * r))
  | _ -> assert false

let add_const ident atoms =
  function
  | Atom atom -> Hashtbl.add atoms ident atom
  | BinOp (op, AtomInt l, AtomInt r) ->
    (
      match eval_op op l r with
      | Some atom -> Hashtbl.add atoms ident atom
      | None -> ()
    )
  | _ -> ()

let collect_const candidates atoms =
  function
  | InstLet (ident, atom_op) when StringSet.mem ident candidates ->
    add_const ident atoms atom_op
  | _ -> ()

let replace_consts_atom consts =
  function
  | AtomInt _ | AtomStr _ as atom -> atom
  | AtomIdent ident as atom ->
    (
      match Hashtbl.find_opt consts ident with
      | Some atom -> atom
      | None -> atom
    )

let rec replace_consts_atom_op consts =
  function
  | Atom atom -> Atom (replace_consts_atom consts atom)

  | BinOp (OpAdd, AtomInt 0, atom) | BinOp (OpAdd, atom, AtomInt 0) ->
    Atom (replace_consts_atom consts atom)

  | BinOp (OpMul, AtomInt 1, atom) | BinOp (OpMul, atom, AtomInt 1) ->
    Atom (replace_consts_atom consts atom)
  | BinOp (OpMul, AtomInt 0, _) | BinOp (OpMul, _, AtomInt 0) ->
    Atom (AtomInt 0)

  | BinOp (OpMul, AtomInt 2, atom) | BinOp (OpMul, atom, AtomInt 2) ->
    let atom = replace_consts_atom consts atom in
    replace_consts_atom_op consts (BinOp (OpAdd, atom, atom))

  | BinOp (op, l, r) ->
    BinOp (op, replace_consts_atom consts l, replace_consts_atom consts r)

let replace_consts_inst consts =
  function
  | InstLet (ident, atom_op) ->
    InstLet (ident, replace_consts_atom_op consts atom_op)
  | InstUpdate (ident, atom_op) ->
    InstUpdate (ident, replace_consts_atom_op consts atom_op)
  | InstIfNot (atom_op, label) ->
    InstIfNot (replace_consts_atom_op consts atom_op, label)
  | InstParam atom -> InstParam (replace_consts_atom consts atom)
  | InstReturn (Some atom) ->
    InstReturn (Some (replace_consts_atom consts atom))
  | inst -> inst

let rec step_replace consts =
  function
  | [] -> []
  | inst :: insts ->
    let inst =
      match inst with
      | InstLabel _ | InstGoto _ | InstCall _ | InstReturn None ->
        (
          Hashtbl.clear consts;
          inst
        )
      | InstLet (ident, atom_op) ->
        (
          let atom_op = replace_consts_atom_op consts atom_op in
          let inst = InstLet (ident, atom_op) in
          add_const ident consts atom_op;
          inst
        )
      | InstUpdate (ident, atom_op) ->
        (
          let atom_op = replace_consts_atom_op consts atom_op in
          let inst = InstUpdate (ident, atom_op) in
          Hashtbl.remove consts ident;
          add_const ident consts atom_op;
          inst
        )
      | InstIfNot (atom_op, label) ->
        (
          let inst =
            InstIfNot (replace_consts_atom_op consts atom_op, label) in
          Hashtbl.clear consts;
          inst
        )
      | InstParam atom -> InstParam (replace_consts_atom consts atom)
      | InstReturn (Some atom) ->
        (
          let inst = InstReturn (Some (replace_consts_atom consts atom)) in
          Hashtbl.clear consts;
          inst
        ) in
    inst :: step_replace consts insts

let rec optimize insts =
  walk_insts insts;

  let unused = StringSet.diff global.lets global.reads in

  let consts = Hashtbl.create 16 in
  List.iter
    (collect_const (StringSet.diff global.lets global.updates) consts)
    insts;

  if StringSet.is_empty unused && Hashtbl.length consts = 0 then (
    insts
  ) else (
    reset ();
    insts
    |> List.filter_map (remove_unused unused)
    |> List.map (replace_consts_inst consts)
    |> step_replace (Hashtbl.create 16)
    |> optimize
  )
