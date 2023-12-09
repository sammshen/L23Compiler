structure Parse : sig

  val parse : Token.token list -> L23RR.term

end = struct

  structure T = Token 
  structure L = L23RR

  fun early x    = raise Fail (x ^ " ended early")
  fun unclosed x = raise Fail ("unclosed " ^ x)
  fun expected x = raise Fail ("expected " ^ x)
			 
  fun nextType [] = NONE
    | nextType (T.Type T.I :: tl) = SOME (Type.Int, tl)
    | nextType (T.Type T.B :: tl) = SOME (Type.Bool, tl)
    | nextType (T.Type T.U :: tl) = SOME (Type.Unit, tl)
    | nextType (T.LParen :: T.Label l :: tl) = recordType (T.Label l :: tl, [])
    | nextType (T.LParen :: tl) =
       (case nextType tl
	  of SOME (tau1, T.Arrow::tl') =>
	      (case nextType tl'
		 of SOME (tau2, T.RParen::tl'') => SOME (Type.Function (tau1, tau2), tl'')
		  | SOME _ => unclosed "function type"
		  | NONE => early "function type (after arrow)")
	   | SOME _ => expected "arrow"
	   | NONE => early "function type")	 
    | nextType (tok :: _) = raise Fail ("error parsing type at " ^ T.tos tok)
  and recordType (T.Label l :: tl, acc) =
       (case nextType tl
	  of SOME (tau, tl') => recordType (tl', (l,tau)::acc)
	   | NONE => early "record type") 
    | recordType (T.RParen :: tl, acc) = SOME (Type.Record (rev acc), tl)
    | recordType _ = raise Fail "error parsing record type" 
		 
  fun binop T.Plus = L.Add
    | binop T.Minus = L.Sub
    | binop T.Star = L.Mul
    | binop T.LessThan = L.LessThan
    | binop T.DoubleEq = L.Eq
    | binop tok = raise Fail ("expected binary operator, found " ^ T.tos tok)

  fun binopStr T.Plus = "+"
    | binopStr T.Minus = "-"
    | binopStr T.Star = "*"
    | binopStr T.LessThan = "<"
    | binopStr T.DoubleEq = "=="
    | binopStr tok = raise Fail "bug"

  fun next [] = NONE
    | next (T.Int n :: tl) = SOME (L.Int n, tl)
    | next (T.T :: tl) = SOME (L.True, tl)
    | next (T.F :: tl) = SOME (L.False, tl)
    | next (T.LParen :: T.RParen :: tl) = SOME (L.Unit, tl)
    | next (T.LParen :: T.Fix :: tl) =
       (case next tl
	  of SOME (t1, T.RParen::tl') => SOME (L.Fix t1, tl')
	   | SOME _ => unclosed "fix"
	   | NONE => early "fix") 
    | next (T.LParen :: T.Label l :: tl) =
       (case next tl
	  of SOME (t1, T.RParen::tl') => SOME (L.Select (l,t1), tl')
	   | SOME _ => unclosed ("select " ^ l)
	   | NONE => early ("select" ^ l)) 
    | next (T.LParen :: tl) =
       (case next tl
  	  of SOME (t1, tl') =>
	      (case next tl'
		 of SOME (t2, T.RParen::tl'') => SOME (L.App (t1, t2), tl'')
		  | SOME _ => unclosed "application"
		  | NONE => early "application")
	   | NONE => early "application?") 	 
    | next (T.ExclamationPoint :: tl) =
       (case next tl
          of SOME (t1, tl') => SOME (L.Not t1, tl')
	   | NONE => early "not")
    | next (T.LBrack :: T.Label l :: tl) = nextRecord (T.Label l :: tl, [])
    | next (T.LBrack :: T.Lam :: T.Var x :: tl) =
       (case nextType tl
	  of SOME (tau, tl') =>
	      (case next tl'
	 	 of SOME (t, T.RBrack::tl'') => SOME (L.Lam (x, tau, t), tl'')
		  | SOME _ => unclosed "lambda"
		  | NONE => early "lambda")
	   | NONE => early "lambda (after var)") 
    | next (T.LBrack :: tl) =
       (case next tl
          of SOME (t1, T.QuestionMark::tl') =>
	       (case next tl'	
	          of SOME (t2, T.Colon::tl'') =>
		      (case next tl''
		         of SOME (t3, T.RBrack::tl''') => SOME (L.Cond (t1, t2, t3), tl''')
			  | SOME _ => unclosed "conditional"
			  | NONE => early "cond (colon)")
	           | SOME _ => expected "colon"
		   | NONE => early "cond (question mark)")
           | SOME (t1, oper::tl') =>
	      (case next tl'
	         of SOME (t2, T.RBrack::tl'') => SOME ((binop oper) (t1, t2), tl'')
		  | SOME (_, tok::_) => unclosed ("binary operation " ^ binopStr oper)
		  | SOME _ => unclosed "binary operation" 
		  | NONE => early ("binary operation " ^ T.tos oper))
           | _ => early "bracketed expression")
    | next (T.LCurly :: T.Var x :: tl) =
        (case next tl
	   of SOME (t1, tl') =>
	      (case next tl'
	        of SOME (t2, T.RCurly::tl'') => SOME (L.Let (x, t1, t2), tl'')
		 | SOME _ => unclosed "let expression"
		 | NONE => early "let expression")
	    | NONE => early "let")
    | next (T.Var x :: tl) = SOME (L.Var x, tl)
    | next (tok::_) = raise Fail ("unparseable expression at " ^ T.tos tok)
  and nextRecord (T.Label l :: tl, acc) =
       (case next tl
	  of SOME (t, tl') => nextRecord (tl', (l,t)::acc)
	   | NONE => early "record")
    | nextRecord (T.RBrack::tl, acc) = SOME (L.Record (rev acc), tl) 
    | nextRecord _ = raise Fail "error parsing record expression"
			    
  fun parse toks =
    let
      fun lp toks =
	(case next toks
	   of SOME (t, []) => t
	    | SOME _ => raise Fail "extra tokens after first term in program"
	    | NONE => raise Fail "empty program")
    in
      lp toks
    end
		     
end



