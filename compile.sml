structure Compile : sig

  datatype result
    = Value of L23RR.term * Type.typ
    | TypeError of string
    | RuntimeError of string
	
  val code : string -> result
  val file : string -> result

  datatype result_untyped
    = ValueUntyped of L23RR.term
    | RuntimeErrorUntyped of string

  val codeUntyped : string -> result_untyped
  val fileUntyped : string -> result_untyped
			 
end = struct

  structure L = L23RR

  datatype result
    = Value of L.term * Type.typ
    | TypeError of string
    | RuntimeError of string (* you should never see these *)

  fun code program =
    let
      val ast = RecordCheck.check (Parse.parse (Scan.scan program))
    in
     (case TypeCheck.typeof ast
        of tau => ((case Eval.eval ast
		      of v => Value (v, tau))
		    handle Fail msg => RuntimeError msg
			 | _ => RuntimeError "?")) 
      handle Fail msg => TypeError (if msg="" then "?" else msg)
	   | _ => TypeError "?"
    end

  fun file filename =
    let
      val program = raise Fail "ReadFile.toString filename"
    in
      code program
    end

  (* the following are provided for fun, if you want to see programs crash *)

  datatype result_untyped
    = ValueUntyped of L23RR.term
    | RuntimeErrorUntyped of string

  fun codeUntyped program = 
    let
      val ast = Parse.parse (Scan.scan program)
    in
     (case Eval.eval ast
        of v => ValueUntyped v)
     handle Fail msg => RuntimeErrorUntyped msg
	  | _ => RuntimeErrorUntyped "?" 
    end
      
  fun fileUntyped filename =
    let
      val program = raise Fail "ReadFile.toString filename"
    in
      codeUntyped program
    end
     
end
