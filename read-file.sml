structure ReadFile : sig

  val toString : string -> string

end = struct

  fun toString filename =
    let
      val instrm = TextIO.openIn filename
      fun loop _ =
        (case TextIO.inputLine instrm
	   of NONE => ""
	    | SOME line => line ^ loop ())
      val str = loop ()
      val _ = TextIO.closeIn instrm
    in
      str
    end

end
