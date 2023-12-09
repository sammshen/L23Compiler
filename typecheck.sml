structure TypeCheck : sig

(* return true if the first type is a subtype of the second *)
  val subty : Type.typ * Type.typ -> bool

(* for commonSupertype, use the s function from the PDF *)
(* if there isn't a common supertype, return NONE *)
  val commonSupertype : Type.typ * Type.typ -> Type.typ option

  val typeof : L23RR.term -> Type.typ
							
end = struct

  structure L = L23RR
  structure T = Type
  structure E = TypeEnv
		  
  (* chapter 15 of TaPL *)
  fun subty (T.Int, T.Int) = true
    | subty (T.Bool, T.Bool) = true
    | subty (T.Unit, T.Unit) = true
    | subty (T.Function (S1, S2), T.Function (T1, T2)) = (subty (T1, S1) andalso subty (S2, T2))
    | subty (T.Record pairs1, T.Record pairs2) = 
      (* for every (l, t2) in pairs2, find (l, t1) in pairs1 s.t. t1 <: t2*)
      let 
        (* signatures
        val subPairExists : (string * T.typ) * ((string * T.typ) list) -> bool
        val allSubPaired : ((string * T.typ) list) * ((string * T.typ) list) -> bool
        *)
        
        (* given a (l2, t2) from pairs2, check all of pairs1 for a "sub-pair" *)
        fun subPairExists ((l2, t2), []) = false
          | subPairExists ((l2, t2), (l1, t1)::remainingPairs1) = 
              if l1 = l2 then subty (t1, t2)
              else subPairExists ((l2, t2), remainingPairs1)
        
        (* given pairs1 and pairs2, check if every (l2, t2) has a "sub-pair" *)
        fun allSubPaired ([], pairs1) = true
          | allSubPaired ((l2, t2)::remainingPairs2, pairs1) =
              subPairExists ((l2, t2), pairs1) andalso allSubPaired (remainingPairs2, pairs1) 
      in
        allSubPaired (pairs2, pairs1)
      end

      (* we don't have anything akin to a numerical tower so we don't utilize transitivity *)
    | subty _ = false

  (* suboptimal implementation *)
  fun commonSupertype (tau1, tau2) =
    if subty (tau1, tau2) then SOME tau2
    else if subty (tau2, tau1) then SOME tau1
    else NONE

  (* helper holding typing environment *)
  fun typeof' (L.Int n) _ = T.Int
    | typeof' L.True    _ = T.Bool
    | typeof' L.False   _ = T.Bool
    | typeof' L.Unit    _ = T.Unit

    | typeof' (L.Var x) gamma = 
      (case E.lookup (gamma, x)
        of SOME tau => tau
         | NONE     =>  raise Fail ("variable " ^ x ^ " not filled with an argument"))
    | typeof' (L.Lam (x, tau1, t1)) gamma = 
      let 
        val gamma' = E.extend (gamma, x, tau1)
        val tau2 = typeof' t1 gamma'
      in 
        T.Function (tau1, tau2)
      end
    | typeof' (L.App (t1, t2)) gamma = 
      (case typeof' t1 gamma
        of T.Function (tau1, tau2) =>
          let 
            val tau3 = typeof' t2 gamma
          in
            if subty (tau3, tau1) then tau2
            else raise Fail ("argument " ^ (T.tos tau3) ^ " must be a subtype of " ^ (T.tos tau1))
          end
         | _ => raise Fail ((L.tos t1) ^ " must be a function"))
    | typeof' (L.Fix t1) gamma =
      (case typeof' t1 gamma
        of T.Function (tau1, tau2) =>
              if tau1 = tau2 then tau1
              else raise Fail ((L.tos t1) ^ " must input and output the same type")
         | _ => raise Fail ((L.tos t1) ^ " must be a function to use fix"))
    | typeof' (L.Let (x, t1, t2)) gamma =
      let
        val tau1 = typeof' t1 gamma
        val gamma' = E.extend (gamma, x, tau1)
        val tau2 = typeof' t2 gamma'
      in
        tau2
      end

    | typeof' (L.Cond (t1, t2, t3)) gamma = 
      let 
        val tau1 = typeof' t1 gamma
        val tau2 = typeof' t2 gamma
        val tau3 = typeof' t3 gamma
      in
        if tau1 = T.Bool then 
          (case commonSupertype (tau2, tau3)
            of SOME tau4 => tau4
             | NONE => raise Fail ((T.tos tau2) ^ " and " ^ (T.tos tau3) 
                          ^ " have no a common supertype"))
        else raise Fail "first term in a conditional must be a boolean"
      end

    | typeof' (L.Add (t1, t2)) gamma = arithType (gamma, t1, t2, "addition")
    | typeof' (L.Sub (t1, t2)) gamma = arithType (gamma, t1, t2, "multiplication")
    | typeof' (L.Mul (t1, t2)) gamma = arithType (gamma, t1, t2, "subtraction")
    | typeof' (L.Eq (t1, t2)) gamma = relType (gamma, t1, t2, "=")
    | typeof' (L.LessThan (t1, t2)) gamma = relType (gamma, t1, t2, "<")
    | typeof' (L.Not t1) gamma = 
      if (typeof' t1 gamma) = T.Bool then T.Bool
      else raise Fail "NOT must be used on a boolean"
    
    (* guaranteed a non-empty record from record checking *)
    | typeof' (L.Record termPairs) gamma = 
      let 
        fun termsToTypes ([], typePairs) = typePairs
          | termsToTypes ((l, t)::termPairs, typePairs) =
            let
              val newTypePair = (l, typeof' t gamma)
            in
              termsToTypes (termPairs, typePairs@[newTypePair])
            end
      in
        T.Record (termsToTypes (termPairs, []))
      end
    | typeof' (L.Select (l, t1)) gamma = 
      (case typeof' t1 gamma 
        of T.Record pairs =>      
          let
            fun findLabel (l, []) = raise Fail ("label " ^ l ^ " not found")
              | findLabel (l, (l', t)::pairs) =
                if l = l' then t
                else findLabel (l, pairs)
          in
            findLabel (l, pairs)
          end
        | _ => raise Fail "SELECT must be used on a record")

    and arithType (gamma, t1, t2, arithOp) = 
      (case (typeof' t1 gamma, typeof' t2 gamma)
        of (T.Int, T.Int) => T.Int
         | _ => raise Fail (arithOp ^ " must be performed on two integers"))
    
    and relType (gamma, t1, t2, relOp) =
      (case (typeof' t1 gamma, typeof' t2 gamma)
        of (T.Int, T.Int) => T.Bool
         | _ => raise Fail (relOp ^ " must be performed on two integers"))

  (* typing environment not exposed in API *)
  fun typeof t = typeof' t E.empty
    
end
