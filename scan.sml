structure Scan : sig

  val scan : string -> Token.token list
	    
end = struct

  structure T = Token

  fun digits cs =
    let
      fun finish (acc, chars) =
	(case Int.fromString (implode (rev acc))
	  of SOME n => SOME (T.Int n, chars)
	   | NONE => raise Fail ("BUG digit " ^ (implode (rev acc))))
      fun lp ([], acc) = finish (acc, [])
	| lp (chars as c::cs, acc) =
	    if Char.isDigit c
	    then lp (cs, c::acc)
	    else finish (acc, chars)
    in
      lp (cs, [])
    end

  fun lowers cs =
    let
      fun finish (acc, chars) =
	(case implode (rev acc)
	   of x => (x, chars))
      fun lp ([], acc) = finish (acc, [])
	| lp (chars as c::cs, acc) =
	    if Char.isLower c
	    then lp (cs, c::acc)
	    else finish (acc, chars)
    in
      lp (cs, [])
    end

  fun var cs =
    (case lowers cs
       of (x, cs') => SOME (T.Var x, cs'))

  fun label cs =
    (case lowers cs
       of (s, cs') => SOME (T.Label ("~"^s), cs'))
      
  datatype char_category = Space | Lower | Digit | Tilde | Other
  fun categorize c =
    if Char.isSpace c then Space
    else if Char.isLower c then Lower
    else if Char.isDigit c then Digit
    else if c = #"~" then Tilde
    else Other
						     
      
  fun next [] = NONE
    | next (#"T" :: tl) = SOME (T.T, tl)
    | next (#"F" :: tl) = SOME (T.F, tl)
    | next (#"[" :: tl) = SOME (T.LBrack, tl)
    | next (#"]" :: tl) = SOME (T.RBrack, tl)
    | next (#"(" :: tl) = SOME (T.LParen, tl)
    | next (#")" :: tl) = SOME (T.RParen, tl)
    | next (#"{" :: tl) = SOME (T.LCurly, tl)
    | next (#"}" :: tl) = SOME (T.RCurly, tl)
    | next (#"+" :: tl) = SOME (T.Plus, tl)
    | next (#"-" :: #">" :: tl) = SOME (T.Arrow, tl)
    | next (#"-" :: tl) = SOME (T.Minus, tl)
    | next (#"*" :: tl) = SOME (T.Star, tl)
    | next (#"=" :: #"=" :: tl) = SOME (T.DoubleEq, tl)
    | next (#"<" :: tl) = SOME (T.LessThan, tl)
    | next (#"!" :: tl) = SOME (T.ExclamationPoint, tl)
    | next (#"f" :: #"i" :: #"x" :: tl) = SOME (T.Fix, tl)
    | next (#"l" :: #"a" :: #"m" :: tl) = SOME (T.Lam, tl) 			       
    | next (#"?" :: tl) = SOME (T.QuestionMark, tl)
    | next (#":" :: tl) = SOME (T.Colon, tl)
    | next (#"I" :: tl) = SOME (T.Type T.I, tl)
    | next (#"B" :: tl) = SOME (T.Type T.B, tl)
    | next (#"U" :: tl) = SOME (T.Type T.U, tl) 
     | next (chars as c::cs) =
       (case categorize c
	  of Space => next cs
	   | Lower => var chars
	   | Digit => digits chars
	   | Tilde => label cs 
	   | Other => raise Fail ("scan error: " ^ implode chars))

  fun scan code =
    let
      fun lp cs =
	(case next cs
	   of SOME (tok, cs') => tok :: lp cs'
	    | NONE => [])
    in
      lp (explode code)
    end
      
end
