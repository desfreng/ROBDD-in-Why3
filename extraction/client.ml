
open Robdd.Robdd__BDDType
open Robdd.Robdd__BDD

let hc = create_hctable ()
let _b = create_node hc Z.zero Top Bottom

let and_op _x _y = assert false (*TODO*)

let implies _x _y =
  assert false (*TODO*)

(* forall i. lo <= i < hi -> f i *)
let rec forall (lo: int) (hi: int) (f: int -> bdd) : bdd =
  if lo >= hi then Top else and_op (f lo) (forall (lo+1) hi f)

let queens _n =
  assert false (*TODO*)

let () = assert (queens 1 != Bottom)
let () = assert (queens 2 == Bottom)
let () = assert (queens 3 == Bottom)
