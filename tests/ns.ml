module M1 = Ns_M1
module M2 = Ns_M2

let print () = print_endline "Hello from NS"

type t = int
type 'a u = 'a list

type 'a opt = Som of 'a | Non

type record = {
  p : 'a. 'a -> 'a;
  o : < m : int -> int >;
  v : [ `A | `B of int ];
  c : int opt;
}

type 'a v = 'a constraint 'a = int

type r = A of r' | B
and r' = C of r | D
