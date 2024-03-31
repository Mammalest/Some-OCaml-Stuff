open Printf
open Terms
open Subst
open Beta

let main: unit =
  let t = Lambda("x", Var "t") in
  printTerm t;
  let l = ["a"; "a"; "b"; "a"; "c"; "b"] in
  let l' = unique l in
  printList l';
  let t =
  Lambda
  (
    "x",
    Lambda
    (
      "y",
      App
      (
        App
        (
          Lambda
          (
            "z",
            Lambda
            (
              "v",
              App(Var "z", App(Var "z", Var "v"))
            )
          ),
          App(Var "x", Var "y")
        ),
        App(Var "z", Var "u")
      )
    )
  )
  in
  let tv = fv t in
  printList tv;
  let f = Lambda("x", App(Var "x", Var "t")) in
  printTerm f;
  let f' = alpha "w" f in
  printTerm f'; 
  let f = Lambda("x", App(Var "x", Var "x")) in
  printTerm f;
  let f' = alpha "y" f in
  printTerm f'; 
  let f = App(Lambda("y", Var "y"), Var "x") in
  printTerm f;
  let f' = alpha "x" f in
  printTerm f'; 
  printf "%s\n" (freshVar "z");
  let t = 
  Lambda
  (
    "x",
    Lambda
    (
      "y",
      App(App(Var "x", Var "y"), Var "z")
    )
  )
  in
  printTerm t;
  let t' = subst t "z" (Var "y") in
  printTerm t';
  let t = Lambda("x", Var "x") in
  let t' = App(t, t) in
  let t'' = beta t' in
  printTerm t'';
  let t = Lambda("x", App(Var "x", Var "x")) in
  let t' = App(t, t) in
  let t'' = beta t' in
  printTerm t'';
  let t'' = beta t'' in
  printTerm t'';
  let t1 =
  Lambda
  (
    "x",
    Lambda
    (
      "y",
      Lambda
      (
        "z",
        App(App(Var "x",Var "z"),App(Var "y", Var "z"))
      )
    )
  ) in
  let t2 = Lambda("x", Var "x") in
  let t3 = App(t1, t2) in
  let t3' = refl_trans_beta t3 in
  printTerm t3';