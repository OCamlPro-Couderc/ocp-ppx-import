module M1 = Ns_M1
module M2 = Ns_M2

let print () = print_endline "Hello from NS"

type t = int
type 'a u = 'a list

type 'a opt = Som of 'a | Non
