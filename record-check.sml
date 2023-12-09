structure RecordCheck : sig

(* check for pairwise distinct labels at all levels of record expressions and record types *)
(* also, reject empty records if you encounter them *)

(* raise an exception if the term doesn't pass the check *)

(* otherwise, return the term as is *)

  val check : L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR

  fun check (L.Record []) = raise Fail "records must not be empty"
    | check (L.Record nonEmptyRecord) = 
      let 
        (* strInList : string * (string list) -> bool *)
        fun strInList (str, []) = false
          | strInList (str, (x::xs)) = str = x orelse strInList (str, xs)

        (* distinctLabels : ((string * term) list) * (string list) -> bool *)
        fun distinctLabels ([], _) = true
          | distinctLabels ((label, t1)::remLabels, currLabels) = 
              
              (* t1 may also be a record: recursive check *)
              check t1 = t1 

              andalso not (strInList (label, currLabels))
              andalso distinctLabels (remLabels, label::currLabels)
      in
       if distinctLabels (nonEmptyRecord, []) then L.Record nonEmptyRecord
       else raise Fail "records cannot have repeated labels"
      end

      (* handle recursive terms *)
    | check (L.Lam (var, tau, t1)) = L.Lam (var, tau, check t1)
    | check (L.App (t1, t2)) = L.App (check t1, check t2)
    | check (L.Fix t1) = L.Fix (check t1)
    | check (L.Let (var, t1, t2)) = L.Let (var, check t1, check t2) 
    | check (L.Cond (t1, t2, t3)) = L.Cond (check t1, check t2, check t3)
    | check (L.Add (t1, t2)) = L.Add (check t1, check t2) 
    | check (L.Sub (t1, t2)) = L.Sub (check t1, check t2) 
    | check (L.Mul (t1, t2)) = L.Mul (check t1, check t2) 
    | check (L.Eq (t1, t2)) = L.Eq (check t1, check t2)
    | check (L.LessThan (t1, t2)) = L.LessThan (check t1, check t2) 
    | check (L.Not t1) = L.Not (check t1)
    | check (L.Select (label, t1)) = L.Select (label, check t1)

      (* return non-recursive terms instantly *)
    | check nonRecursiveTerm = nonRecursiveTerm

end
