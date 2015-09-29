[%%namespace MyNs]
[%%import Ns.(M1 => M1', M2, print, opt [@type])]
[%%import Map.(Make, S[@type])]

let p1 = M1.print
let p2 = M2.print

let _ =
  M1.print ();
  M1'.print ();
  M2.print ()
