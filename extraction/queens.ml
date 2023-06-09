open Robdd.Robdd__BDDType
open Robdd.Robdd__BDD
open Robdd.Robdd__Size
open Robdd.Robdd__Sat
open Robdd.Robdd__AnySat
open Robdd.Robdd__CountSat

let hc = create_hctable ()
let and_memo = Robdd.Robdd__And.init_memo_map hc
let or_memo = Robdd.Robdd__Or.init_memo_map hc
let implies_memo = Robdd.Robdd__Implies.init_memo_map hc
let not_memo = Robdd.Robdd__Not.init_memo_map hc

(* And operation  *)
let ( &&& ) a b = Robdd.Robdd__And.apply and_memo a b

(* Not operation *)
let ( ! ) a = Robdd.Robdd__Not.apply not_memo a

(* Or operation *)
let ( ||| ) a b = Robdd.Robdd__Or.apply or_memo a b

(* Implication *)
let ( --> ) a b = Robdd.Robdd__Implies.apply implies_memo a b

(* forall i. lo <= i < hi -> f i *)
let rec forall lo hi f = if lo >= hi then Top else f lo &&& forall (lo + 1) hi f

(* forall i. lo <= i < hi -> f i *)
let rec exists lo hi f =
  if lo >= hi then Bottom else f lo ||| exists (lo + 1) hi f

let var_id_of_tuple n (i, j) = i + (n * j)

(* Queen at (i, j) *)
let v n i j =
  let id = var_id_of_tuple n (i, j) in
  create_node hc id Top Bottom

(* forall i. 0 <= i < n, exists j. 0 <= j < n, V i j *)
let one_queen_per_line n = forall 0 n (fun i -> exists 0 n (fun j -> v n i j))

(* forall i. 0 <= i < n, forall j. 0 <= j < n, forall j'. 0 <= j' < n, j != j' -> V i j -> !V i j' *)
let no_queens_same_colunm n =
  forall 0 n (fun i ->
      forall 0 n (fun j ->
          forall 0 n (fun j' ->
              if j != j' then v n i j --> !(v n i j') else Top)))

(* forall j. 0 <= j < n, forall i. 0 <= i < n, forall i'. 0 <= i' < n, i != i' -> V i j -> !V i' j *)
let no_queens_same_line n =
  forall 0 n (fun j ->
      forall 0 n (fun i ->
          forall 0 n (fun i' ->
              if i != i' then v n i j --> !(v n i' j) else Top)))

(* forall i. 0 <= i < n, forall j. 0 <= j < n,
      forall d. max(-i, -j) <= d < min(n - i, n - j), d != 0 -> V i j -> !V (i + d) (j + d) *)
let no_queens_same_diagonal n =
  forall 0 n (fun i ->
      forall 0 n (fun j ->
          forall (max (-i) (-j))
            (min (n - i) (n - j))
            (fun d ->
              if d != 0 then v n i j --> !(v n (i + d) (j + d)) else Top)))

(* forall i. 0 <= i < n, forall j. 0 <= j < n,
      forall d. max(-i, n - j + 1) <= d < min(n - i, j + 1), V i j -> !V (i + d) (j - d) *)
let no_queens_same_anti_diagonal n =
  forall 0 n (fun i ->
      forall 0 n (fun j ->
          forall
            (max (-i) (j - n + 1))
            (min (n - i) (j + 1))
            (fun d ->
              if d != 0 then v n i j --> !(v n (i + d) (j - d)) else Top)))

let queens n =
  one_queen_per_line n &&& no_queens_same_line n &&& no_queens_same_colunm n
  &&& no_queens_same_diagonal n
  &&& no_queens_same_anti_diagonal n

let print_sol n s =
  let line_sep =
    String.init ((4 * n) + 1) (fun i -> if i mod 4 = 0 then '+' else '-')
  in
  Format.printf "%s\n" line_sep;
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if s.(var_id_of_tuple n (i, j)) then Format.printf "| X "
      else Format.printf "|   "
    done;
    Format.printf "|\n%s\n" line_sep
  done;
  Format.print_flush ()

let rec string_of_bdd a =
  match a with
  | Top -> "Top"
  | Bottom -> "Bottom"
  | N (x, t, f, _) ->
      Format.sprintf "Node(%i, %s, %s)" x (string_of_bdd t) (string_of_bdd f)

let () =
  Format.printf "@.Small Tests : @.";
  for i = 0 to 7 do
    let q = queens i in
    Format.printf "Test on %i-Queens : @." i;
    if i < 5 then Format.printf "BDD : %s@." (string_of_bdd q);
    Format.printf "Is-Sat : %b@." (is_sat q);
    Format.printf "Number of solution : %s@."
      (count_sat (i * i) q |> Z.to_string);
    Format.print_newline ()
  done

let () =
  Format.printf "Enter N : @?";
  let n = read_int () in
  let t1 = Sys.time () in
  let b = queens n in
  let t = Sys.time () -. t1 in
  if is_sat b then (
    Format.printf "The %i-Queens Problem is satisfiable : @." n;
    let sol = any_sat (n * n) b in
    print_sol n sol;
    Format.printf "Number of Solution : %s @."
      (count_sat (n * n) b |> Z.to_string))
  else Format.printf "The %i-Queens Problem have no solution@." n;
  Format.printf "@.Number of distinct nodes of the BDD : %i@." (size b);
  Format.printf "Time passed to build the BDD : %f s@." t
