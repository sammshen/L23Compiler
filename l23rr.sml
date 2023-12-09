structure L23RR = struct

  datatype term
    = Int of int
    | True
    | False
    | Unit
    | Var of string
    | Lam of string * Type.typ * term
    | App of term * term
    | Fix of term
    | Let of string * term * term
    | Cond of term * term * term
    | Add of term * term
    | Sub of term * term
    | Mul of term * term
    | Eq of term * term
    | LessThan of term * term
    | Not of term
    | Record of (string * term) list
    | Select of string * term

  infix +-+
  fun s1 +-+ s2 = s1 ^ " " ^ s2

  val catw = String.concatWith
	       
  fun tos (Int n) = Int.toString n
    | tos True = "T"
    | tos False = "F"
    | tos Unit = "()"
    | tos (Var x) = x
    | tos (Lam (x, tau, t1)) = brack ("lam" +-+ x +-+ Type.tos tau +-+ tos t1)
    | tos (App (t1, t2)) = par (tos t1 +-+ tos t2)
    | tos (Fix t1) = par ("fix" +-+ tos t1)
    | tos (Let (x, t1, t2)) = brace ("let " ^ x +-+ tos t1 +-+ tos t2)
    | tos (Cond (t1, t2, t3)) = brack (tos t1 ^ "?" ^ tos t2 ^ ":" ^ tos t3)
    | tos (Add (t1, t2)) = brack (tos t1 ^ "+" ^ tos t2)
    | tos (Sub (t1, t2)) = brack (tos t1 ^ "-" ^ tos t2)
    | tos (Mul (t1, t2)) = brack (tos t1 ^ "*" ^ tos t2)
    | tos (Eq (t1, t2)) = brack (tos t1 ^ "==" ^ tos t2)
    | tos (LessThan (t1, t2)) = brack (tos t1 ^ "<" ^ tos t2)
    | tos (Not t1) = "!" ^ tos t1
    | tos (Record pairs) = par (catw " " (map (fn (l,t) => l +-+ tos t) pairs))
    | tos (Select (label, t1)) = par (label +-+ tos t1) 
  and par s   = "(" ^ s ^ ")"
  and brack s = "[" ^ s ^ "]"
  and brace s = "{" ^ s ^ "}"
			    
end

