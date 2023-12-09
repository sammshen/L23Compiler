structure Token = struct

  datatype ground_type = I | B | U

  datatype token
    = Int of int
    | T
    | F
    | LBrack
    | RBrack
    | LParen
    | RParen
    | LCurly
    | RCurly
    | Plus
    | Minus
    | Star
    | DoubleEq
    | LessThan
    | ExclamationPoint
    | Var of string
    | Label of string (* starts with ~ *)
    | Fix
    | Lam
    | QuestionMark
    | Colon
    | Type of ground_type
    | Arrow
	
  fun tos (Int n) = "Int(" ^ Int.toString n ^ ")"
    | tos T = "T"
    | tos F = "F"
    | tos LBrack = "["
    | tos RBrack = "]"
    | tos LParen = "("
    | tos RParen = ")"
    | tos LCurly = "{"
    | tos RCurly = "}"
    | tos Plus = "+"
    | tos Minus = "-" 
    | tos Star = "*"
    | tos DoubleEq = "=="
    | tos LessThan = "<" 
    | tos ExclamationPoint = "!"
    | tos (Var x) = x
    | tos (Label l) = l
    | tos Fix = "fix"
    | tos Lam = "lam"
    | tos QuestionMark = "?"
    | tos Colon = ":"
    | tos (Type I) = "I"
    | tos (Type B) = "B"
    | tos (Type U) = "U" 
    | tos Arrow = "->"
 		    
end
