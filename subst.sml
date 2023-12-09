structure Subst : sig

  val subst : string * L23RR.term * L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR

(* note: We will do without VarSet/FV this time. *)
(* This is because free variables are effectively banished by the typechecker. *)
(* That is, we shouldn't have them when we get to evaluation. *)
		  
  fun subst (x, subst_term, original_term) =
    (case original_term
      (* recursive cases *)
      of L.Var y => 
        if x = y then subst_term 
        else original_term
      | L.Lam (y, tau, t1) => 
        if x = y then original_term
        else L.Lam (y, tau, subst (x, subst_term, t1))
      | L.App (t1, t2) => L.App (doubleSubst (x, subst_term, t1, t2))
      | L.Fix t1 => L.Fix (subst (x, subst_term, t1))
      | L.Let (y, t1, t2) => 
        if x = y then L.Let (y, subst (x, subst_term, t1), t2)
        else L.Let (y, subst (x, subst_term, t1), subst (x, subst_term, t2))
      | L.Cond (t1, t2, t3) => 
        L.Cond (subst (x, subst_term, t1), subst (x, subst_term, t2), subst (x, subst_term, t3))
      | L.Add (t1, t2) => L.Add (doubleSubst (x, subst_term, t1, t2))
      | L.Sub (t1, t2) => L.Sub (doubleSubst (x, subst_term, t1, t2))
      | L.Mul (t1, t2) => L.Mul (doubleSubst (x, subst_term, t1, t2))
      | L.Eq (t1, t2) => L.Eq (doubleSubst (x, subst_term, t1, t2))
      | L.LessThan (t1, t2) => L.LessThan (doubleSubst (x, subst_term, t1, t2))
      | L.Not t1 => L.Not (subst (x, subst_term, t1))
      | L.Record pairs => 
        let 
          fun pairSubst ([], subbedPairs) = subbedPairs
            | pairSubst ((l, t)::pairs, subbedPairs) = 
                pairSubst (pairs, subbedPairs@[(l, subst (x, subst_term, t))])
        in
          L.Record (pairSubst (pairs, []))
        end
      | L.Select (l, record) => L.Select (l, subst (x, subst_term, record))
      (* non-recursive cases *)
      | _ => original_term
      )
    and doubleSubst (x, subst_term, t1, t2) = 
      (subst (x, subst_term, t1), subst (x, subst_term, t2))

end
