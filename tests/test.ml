[%%namespace MyNs]
[%%import Ns.(M1 => M1',
              M2,
              S [@type],
              print,
              opt => opt' [@type],
              record [@type],
              v [@type],
              r => r' [@type],
              r' => r [@type]
             )]
(* [%%import Ns.(t [@type], u [@type])] *)
let p1 = M1.print
let p2 = M2.print

let _ =
  M1.print ();
  M1'.print ();
  M2.print ()

let x = A (C (A (C (B))))

module M = struct
  [%%include Map.(Make, S[@type])]
  [%%include Pervasives.compare]
end
