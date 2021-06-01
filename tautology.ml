(* Calvin Hoffmann *)
(* March 23 2021 *)
(* Professor Moen *)

type proposition = 
    False | 
    True | 
    Var of string | 
    And of proposition * proposition | 
    Or of proposition * proposition | 
    Not of proposition | 
    Imply of proposition * proposition | 
    Equiv of proposition * proposition ;;

type conditional = 
    IffyFalse | 
    IffyTrue | 
    IffyVar of string | 
    If of conditional * conditional * conditional ;;

let rec ifify p = 
    match p with 

    | False -> IffyFalse
    | True -> IffyTrue
    | Var a -> IffyVar a
    | Not a -> If((ifify a),  IffyFalse, IffyTrue)
    | And(a, b) -> If((ifify a), ifify b, IffyFalse)
    | Or(a, b) -> If((ifify a), IffyTrue, ifify b)
    | Imply(a, b) -> If((ifify a), ifify b, IffyTrue)
    | Equiv(a, b) -> If((ifify a), ifify b, If(ifify b, IffyFalse, IffyTrue))

;;

let rec normalize c = 
    match c with 
    | If(p, a, b) -> 
        let rec normalizing p a b =                 (*normalize helper to take care of recursion*)
            match p with 
            | If(p, a1, b1) -> normalizing p (If(a1, a, b)) (If(b1, a, b))
            | _ -> If(p, normalize a, normalize b)
        in normalizing p a b 
    | _ -> c
;;
(* (IF (IF π α1 β1) α2 β2)   ⇒   (IF π (IF α1 α2 β2) (IF β1 α2 β2))  *)

let rec substitute c v b = 
    match c with 
    | If(c1, v1, b1) -> If(substitute c1 v b, substitute v1 v b, substitute b1 v b)

    | x -> 
        if x = v
            then b
        else
            x
;;

let rec simplify c = 
    match c with 
    | If(IffyTrue, a, b) -> simplify a

    | If(IffyFalse, a, b) -> simplify b

    | If(p, IffyTrue, IffyFalse) -> p

    | If(p, a, b) ->                (*combined rule 10 and 11 to make it easier*)
        if simplify (substitute a p IffyTrue) = simplify (substitute b p IffyFalse)
            then simplify (substitute a p IffyTrue)
        else (If(p, simplify (substitute b p IffyFalse), simplify (substitute a p IffyTrue)) )

    | _ -> c
;;

 (* (IF π α β)  ⇒ (IF π α{π ⇒ true} β{π ⇒ false})  *)


let tautology p = 
    let value = simplify(normalize(ifify(p))) in
    match value with 
    | IffyTrue -> true
    | IffyFalse -> false
    | If(_,_,_) -> false        (*unsimplifiable If*)

    | IffyVar x -> false        (*should never happen*)


;;


(* #use "tautology.ml";; *)


(* Q1. A test case. This is ¬ (α ∧ β) → (¬ α ∨ ¬ β). It's a tautology. *)
 
(* let q1 =
  (Imply
    (Not
      (And
        (Var "p", Var "q")),
     Or
      (Not
        (Var "p"),
       Not
        (Var "q")))) ;;

tautology q1;; *)

(* Q2. A test case. This is α ∧ β. It's not a tautology. *)

(* let q2 =
  (And
    (Var "p", Var "q")) ;;

tautology q2;; *)

(* Beware: Q1 and Q2 are not exhaustive tests! Your code is not necessarily
   correct if it works on them. *)

(*OUTPUT*)

(*# #use "tautology.ml";;
type proposition =
    False
  | True
  | Var of string
  | And of proposition * proposition
  | Or of proposition * proposition
  | Not of proposition
  | Imply of proposition * proposition
  | Equiv of proposition * proposition
type conditional =
    IffyFalse
  | IffyTrue
  | IffyVar of string
  | If of conditional * conditional * conditional
val ifify : proposition -> conditional = <fun>
val normalize : conditional -> conditional = <fun>
val substitute : conditional -> conditional -> conditional -> conditional =
  <fun>
val simplify : conditional -> conditional = <fun>
File "tautology.ml", lines 82-85, characters 4-24:
82 | ....match value with 
83 |     | IffyTrue -> true
84 |     | IffyFalse -> false
85 |     | If(_,_,_) -> false.............................
val tautology : proposition -> bool = <fun>
val q1 : proposition =
  Imply (Not (And (Var "p", Var "q")), Or (Not (Var "p"), Not (Var "q")))
- : bool = true
val q2 : proposition = And (Var "p", Var "q")
- : bool = false*)