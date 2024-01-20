open Ast
open Ir
open Compile
open Optimize

let () =
  let funcs =
    [
      ("println_i64",
       ["x"],
       [
         StmtVoid (ExprBinOp (OpAdd, ExprInt 0, ExprInt 1));
         StmtVoid
           (ExprCall (ExprIdent "printf", [ExprStr "%ld\n"; ExprIdent "x"]));
         StmtReturn None;
       ]);
      ("main",
       [],
       [
         StmtLet ("x", ExprInt 1);
         StmtLet ("i", ExprInt 0);
         StmtLet ("m", ExprInt 7);
         StmtLet
           ("n",
            ExprBinOp
              (OpAdd,
               ExprBinOp (OpAdd, ExprIdent "m", ExprInt 2),
               ExprIdent "x"));
         StmtLoop
           [
             StmtLet ("k", ExprBinOp (OpMul, ExprInt 2, ExprInt 1));
             StmtIf
               (ExprBinOp (OpGe, ExprIdent "i", ExprIdent "n"), [StmtBreak]);
             StmtVoid (ExprCall (ExprIdent "println_i64", [ExprIdent "x"]));
             StmtUpdate
               (ExprIdent "x",
                ExprBinOp (OpMul, ExprIdent "x", ExprIdent "k"));
             StmtLet ("z", ExprInt 123);
             StmtIf
               (ExprBinOp
                  (OpGe, ExprIdent "x", ExprIdent "z"),
                [StmtReturn (Some (ExprIdent "x"))]);
             StmtUpdate
               (ExprIdent "i", ExprBinOp (OpAdd, ExprIdent "i", ExprInt 1));
           ];
         StmtReturn (Some (ExprInt 0));
       ])
    ] in

  funcs
  |> List.map (show_func 0)
  |> List.iter print_endline;

  funcs
  |> List.map func_to_insts
  |> List.map optimize
  |> List.map show_insts
  |> List.iter print_endline;

  flush stdout
