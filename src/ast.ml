type op =
  | OpAdd
  | OpMul
  | OpGe

type expr =
  | ExprInt of int
  | ExprStr of string
  | ExprIdent of string
  | ExprAddr of string
  | ExprDeref of expr
  | ExprBinOp of (op * expr * expr)
  | ExprCall of expr * (expr list)

type stmt =
  | StmtVoid of expr
  | StmtLet of string * expr
  | StmtUpdate of expr * expr
  | StmtLoop of scope
  | StmtIf of expr * scope
  | StmtBreak
  | StmtReturn of expr option

and scope = stmt list

and func = string * (string list) * scope

let indent n =
  String.make n ' '

let show_op =
  function
  | OpAdd -> "+"
  | OpMul -> "*"
  | OpGe -> ">="

let rec show_expr n =
  function
  | ExprInt n -> string_of_int n
  | ExprStr str -> Printf.sprintf "%S" str
  | ExprIdent ident -> ident
  | ExprAddr ident -> Printf.sprintf "(& %s)" ident
  | ExprDeref expr -> Printf.sprintf "(* %s)" (show_expr n expr)
  | ExprBinOp (op, l, r) ->
    Printf.sprintf "(%s %s %s)" (show_op op) (show_expr n l) (show_expr n r)
  | ExprCall (func, args) ->
    List.map (show_expr n) args
    |> List.cons (show_expr n func)
    |> String.concat " "
    |> Printf.sprintf "(%s)"

and show_scope n stmts =
  let inner =
    List.map (show_stmt (n + 4)) stmts
    |> List.map (Printf.sprintf "%s\n")
    |> String.concat "" in
  Printf.sprintf "{\n%s%s}" inner (indent n)

and show_stmt n =
  function
  | StmtVoid expr -> show_expr n expr |> Printf.sprintf "%s%s;" (indent n)
  | StmtLet (ident, expr) ->
    Printf.sprintf "%s%s := %s;" (indent n) ident (show_expr n expr)
  | StmtUpdate (target, value) ->
    Printf.sprintf
      "%s%s = %s;"
      (indent n)
      (show_expr n target)
      (show_expr n value)
  | StmtLoop scope ->
    show_scope n scope
    |> Printf.sprintf "%sloop %s" (indent n)
  | StmtIf (condition, scope) ->
    show_scope n scope
    |> Printf.sprintf "%sif %s %s" (indent n) (show_expr n condition)
  | StmtBreak -> Printf.sprintf "%sbreak;" (indent n)
  | StmtReturn (Some expr) ->
    show_expr n expr
    |> Printf.sprintf "%sreturn %s;" (indent n)
  | StmtReturn None -> Printf.sprintf "%sreturn;" (indent n)

let show_func n (ident, args, scope) =
  Printf.sprintf
    "%s %s\n"
    (String.concat " " (ident :: args))
    (show_scope n scope)
