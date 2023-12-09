structure Type = struct

  datatype typ
    = Int
    | Bool
    | Unit
    | Function of typ * typ
    | Record of (string * typ) list

  val catw = String.concatWith
  fun tos Int = "I"
    | tos Bool = "B"
    | tos Unit = "U"
    | tos (Function (tau1, tau2)) = "(" ^ tos tau1 ^ " -> " ^ tos tau2 ^ ")"
    | tos (Record pairs) = par (catw " " (map (fn (l,tau) => l ^ " " ^ tos tau) pairs))
  and par s = "(" ^ s ^ ")"

end
