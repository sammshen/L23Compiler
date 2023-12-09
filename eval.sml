structure Eval : sig

  val eval : L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR
  val subst = Subst.subst
  
  fun isV (L.Int _) = true
    | isV L.Unit = true
    | isV L.True = true
    | isV L.False = true
    | isV (L.Lam _) = true
    | isV (L.Record pairs) = 
      let
        fun checkAllVals [] = true
          | checkAllVals ((l, t)::pairs) = isV t andalso checkAllVals pairs
      in
        checkAllVals pairs
      end
    | isV _ = false

        
  fun eval (L.Int n) = L.Int n
    | eval L.True = L.True
    | eval L.False = L.False
    | eval L.Unit = L.Unit
    | eval (L.Var x) = L.Var x
    | eval (L.Lam (x, tau1, t1)) = L.Lam (x, tau1, t1)
 
    | eval (L.App (t1, t2)) = 
      let 
        val v2 = eval t2
      in
        if not (isV v2) then raise Fail "application argument must be a value"
        else 
          (case eval t1
            of L.Lam (x, tau1, t11) => eval (subst (x, v2, t11))
             | _ => raise Fail "application must be used with a function")
      end
    | eval (L.Fix t1) = 
      (case eval t1
        of L.Lam (f, tau1, t11) => 
            eval (subst (f, L.Fix (L.Lam (f, tau1, t11)), t11))
         | _ => raise Fail "FIX must be used on a function")
    | eval (L.Let (x, t1, t2)) = 
      let 
        val v1 = eval t1
      in
        if not (isV v1) then raise Fail ("argument " ^ (L.tos t1) ^ " must be a value")
        else eval (subst (x, v1, t2))
      end
    | eval (L.Cond (t1, t2, t3)) = 
      (case eval t1
        of L.True => eval t2
         | L.False => eval t3
         | _ => raise Fail "typechecking ensures this doesn't run")
      (* already type checked at this point *)
    | eval (L.Add (t1, t2)) =
      (case (eval t1, eval t2) 
        of (L.Int n1, L.Int n2) => L.Int (n1 + n2)
         | _ => raise Fail "typchecking ensures this doesn't run")
    | eval (L.Sub (t1, t2)) =
      (case (eval t1, eval t2) 
        of (L.Int n1, L.Int n2) => L.Int (n1 - n2)
         | _ => raise Fail "typchecking ensures this doesn't run")
    | eval (L.Mul (t1, t2)) =
      (case (eval t1, eval t2) 
        of (L.Int n1, L.Int n2) => L.Int (n1 * n2)
         | _ => raise Fail "typchecking ensures this doesn't run")
    | eval (L.Eq (t1, t2)) =
      (case (eval t1, eval t2) 
        of (L.Int n1, L.Int n2) => if n1 = n2 then L.True else L.False
         | _ => raise Fail "typchecking ensures this doesn't run")
    | eval (L.LessThan (t1, t2)) =
      (case (eval t1, eval t2) 
        of (L.Int n1, L.Int n2) => if n1 < n2 then L.True else L.False
         | _ => raise Fail "typchecking ensures this doesn't run")
    | eval (L.Not t1) =
      (case eval t1 
        of L.True => L.False
         | L.False => L.True
         | _ => raise Fail "typechecking ensures this doesn't run")
    | eval (L.Record pairs) = 
      let
        fun evalPairs ([], evaledPairs) = evaledPairs
         | evalPairs ((l, t)::pairs, evaledPairs) = 
            evalPairs (pairs, evaledPairs@[(l, eval t)])
        val evaledRecord = L.Record (evalPairs (pairs, []))
      in
        if isV evaledRecord then evaledRecord
        else raise Fail "all fields of the record must be values" 
      end
    | eval (L.Select (l, record)) = 
      let
          (* demand that the whole record successfully evaluates first *)
        val evaledRecord = eval record
        fun findLabel (l, []) = raise Fail ("label " ^ l ^ " not in record " ^ (L.tos record))
          | findLabel (l, (l1, t1)::pairs) = 
            if l = l1 then t1
            else findLabel (l, pairs)
      in
        (case evaledRecord 
          of L.Record evaledPairs => findLabel (l, evaledPairs)
           | _ => raise Fail "typechecking ensures select is used on a record")
      end		 
end
