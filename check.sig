signature CHECK = sig

  (* note: the final string in the argument to each of these tuples
   * is a label for that test. Each label should be unique for easy
   * identification of which test may have failed. The uniqueness-of-label
   * property is not mechanically enforced; it is a recommendation.
   *)
  
  (* check if given boolean is true *)
  val assertT : bool * string -> unit

  (* check if given boolean is false *)
  val assertF : bool * string -> unit

  (* check if two items are equal by built-in polymorphic equality *)
  val expect : ''a * ''a * string -> unit

  (* check if two items are equal by a supplied equality function *)
  val expectBy : ('a * 'a -> bool) * 'a * 'a * string -> unit

  (* check if two floating-point values are within epsilon of another *)
  (* epsilon is the _first_ real in the tuple *)
  val within : real * real * real * string -> unit

  (* check if given delayed computation raises an exception *)
  val exn : (unit -> 'a) * string -> unit

end
	
