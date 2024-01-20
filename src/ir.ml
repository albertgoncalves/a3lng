type atom =
  | AtomInt of int
  | AtomStr of string
  | AtomIdent of string
  | AtomAddr of string
  | AtomDeref of string

type atom_op =
  | Atom of atom
  | BinOp of Ast.op * atom * atom

type inst =
  | InstLabel of string * (string list option)
  | InstGoto of string
  | InstLet of string * atom_op
  | InstUpdate of string * atom_op
  | InstIfNot of atom_op * string
  | InstParam of atom
  | InstCall of string * (string option)
  | InstReturn of (atom option)

let show_atom =
  function
  | AtomInt n -> string_of_int n
  | AtomStr str -> Printf.sprintf "%S" str
  | AtomIdent ident -> ident
  | AtomAddr ident -> Printf.sprintf "&%s" ident
  | AtomDeref ident -> Printf.sprintf "*%s" ident

let show_atom_op =
  function
  | Atom atom -> show_atom atom
  | BinOp (op, l, r) ->
    Printf.sprintf "%s %s %s" (show_atom l) (Ast.show_op op) (show_atom r)

let show_inst inst =
  (
    match inst with
    | InstLabel (label, Some args) ->
      Printf.sprintf "%s(%s):" label (String.concat ", " args)
    | InstLabel (label, None) -> Printf.sprintf "%s:" label
    | InstGoto label -> Printf.sprintf "    goto %s" label
    | InstLet (ident, value) ->
      Printf.sprintf "    %s := %s" ident (show_atom_op value)
    | InstIfNot (condition, label) ->
      Printf.sprintf "    ifnot %s goto %s" (show_atom_op condition) label
    | InstUpdate (ident, value) ->
      Printf.sprintf "    %s = %s" ident (show_atom_op value)
    | InstParam atom -> Printf.sprintf "    param %s" (show_atom atom)
    | InstCall (label, Some ident) ->
      Printf.sprintf "    %s := call %s" ident label
    | InstCall (label, None) -> Printf.sprintf "    call %s" label
    | InstReturn (Some atom) ->
      Printf.sprintf "    return %s" (show_atom atom)
    | InstReturn None -> "    return"
  )
  |> Printf.sprintf "    %s\n"

let show_insts insts =
  insts
  |> List.map show_inst
  |> String.concat ""
