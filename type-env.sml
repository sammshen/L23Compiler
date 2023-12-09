structure TypeEnv :> sig

  type env

  val empty  : env
  val lookup : env * string -> Type.typ option
  val extend : env * string * Type.typ -> env
	    
end = struct

  type env = (string * Type.typ) list

  val empty = []

  fun lookup ([], _) = NONE
    | lookup ((x,tau)::e, y) = if x=y then SOME tau else lookup (e, y)

  fun extend (gamma, x, tau) = (x,tau)::gamma
					  
end
