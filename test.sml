structure Test = struct

  structure L = L23RR
  structure T = Type

  fun println s = (print s; print "\n")

  fun rch_ x = L.tos(RecordCheck.check(Parse.parse(Scan.scan(x))))
  fun rch () =
    let
      val _ = Check.expect (rch_ "[~a 1 ~b T ~c ()]", "(~a 1 ~b T ~c ())", "rch0")
      val _ = Check.expect (rch_ "[~a [~x 1 ~y 2] ~b [~x 3 ~y 4]]", "(~a (~x 1 ~y 2) ~b (~x 3 ~y 4))", "rch1")
      val _ = Check.expect (rch_ "[~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]", "(~a (~a 1 ~b 2) ~b (~a 3 ~b 4))", "rch2")
      val _ = Check.expect (rch_ "[(~a [~a 1 ~b T ~c ()]) + (~b (~b [~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]))]", "[(~a (~a 1 ~b T ~c ()))+(~b (~b (~a (~a 1 ~b 2) ~b (~a 3 ~b 4))))]", "rch3")

      val _ = Check.exn (fn () => rch_ "[~a 1 ~a T ~c ()]", "badrch0")
      val _ = Check.exn (fn () => rch_ "[~a [~x 1 ~y 2] ~b [~x 3 ~x 4]]", "badrch1")
      val _ = Check.exn (fn () => rch_ "[~a [~a 1 ~b 2] ~a [~a 3 ~b 4]]", "badrch2")
      val _ = Check.exn (fn () => rch_ "[~a [~b 1 ~b 2] ~b [~a 3 ~b 4]]", "badrch3")
    in
      println "record check tests done"
    end

  fun typ_ x = T.tos(TypeCheck.typeof(RecordCheck.check(Parse.parse(Scan.scan(x)))))
  fun typ () =
    let
      val _ = Check.expect (typ_ "0", "I", "typ0")
      val _ = Check.expect (typ_ "3", "I", "typ1")
      val _ = Check.expect (typ_ "T", "B", "typ2")
      val _ = Check.expect (typ_ "F", "B", "typ3")
      val _ = Check.expect (typ_ "()", "U", "typ4")
      val _ = Check.expect (typ_ "[2 + 3]", "I", "typ5") (* 5 *)
      val _ = Check.expect (typ_ "[3 - 2]", "I", "typ6") (* 1 *)
      val _ = Check.expect (typ_ "[2 * 3]", "I", "typ7") (* 6 *)
      val _ = Check.expect (typ_ "[20 - [2 + [2 * [2 * 4]]]]", "I", "typ8") (* 2 *)
      val _ = Check.expect (typ_ "[[8 + 4] < [20 - 2]]", "B", "typ19") (* T *)
      val _ = Check.expect (typ_ "!T", "B", "typ10") (* F *)
      val _ = Check.expect (typ_ "!F", "B", "typ11") (* T *)
      val _ = Check.expect (typ_ "[2 == 3]", "B", "typ12") (* F *)
      val _ = Check.expect (typ_ "[2 == 2]", "B", "typ13") (* T *)
      val _ = Check.expect (typ_ "[T ? T : F]", "B", "typ14") (* T *)
      val _ = Check.expect (typ_ "[F ? F : T]", "B", "typ15") (* T *)
      val _ = Check.expect (typ_ "[T ? 0 : 1]", "I", "typ16") (* 0 *)
      val _ = Check.expect (typ_ "[F ? 1 : 0]", "I", "typ17") (* 0 *)
      val _ = Check.expect (typ_ "{x 1 T}", "B", "typ18") (* T *)
      val _ = Check.expect (typ_ "{x 1 x}", "I", "typ19") (* 1 *)
      val _ = Check.expect (typ_ "{x 2 [x * x]}", "I", "typ20") (* 4 *)
      val _ = Check.expect (typ_ "{x 0 {x T !x}}", "B", "typ21") (* F *)
      val _ = Check.expect (typ_ "[~a 1 ~b T ~c ()]", "(~a I ~b B ~c U)", "typ22")
      val _ = Check.expect (typ_ "[~a [~x 1 ~y 2] ~b [~x 3 ~y 4]]", "(~a (~x I ~y I) ~b (~x I ~y I))", "typ23")
      val _ = Check.expect (typ_ "[~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]", "(~a (~a I ~b I) ~b (~a I ~b I))", "typ24")
      val _ = Check.expect (typ_ "(~a [~a 1 ~b T ~c ()])", "I", "typ25") (* 1 *)
      val _ = Check.expect (typ_ "(~b [~a [~a 1 ~b 2] ~b [~a 3 ~b 4]])", "(~a I ~b I)", "typ26") (* (~a 3 ~b 4) *)
      val _ = Check.expect (typ_ "(~b (~b [~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]))", "I", "typ27") (* 4 *)
      val _ = Check.expect (typ_ "[(~a [~a 1 ~b T ~c ()]) + (~b (~b [~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]))]", "I", "typ28") (* 5 *)
      val _ = Check.expect (typ_ "[T ? [~a 1 ~b 2] : [~b 3]]", "(~b I)", "typ29") (* (~a 1 ~b 2)? *)
      val _ = Check.expect (typ_ "([lam x (~c I) (~c x)] [~a () ~b F ~c 3])", "I", "typ30") (* 3 *)
      val _ = Check.expect (typ_ "[lam x (~a I ~b B) [~a (~b x) ~b (~a x)]]", "((~a I ~b B) -> (~a B ~b I))", "typ31") (* ? *)
      val _ = Check.expect (typ_ "[lam x (~a I ~b B ~c U) [~a (~b x)]]", "((~a I ~b B ~c U) -> (~a B))", "typ32") (* ? *)
      val _ = Check.expect (typ_ "([lam y ((~a I ~b B ~c U) -> (~a B)) ()] [lam x (~a I ~b B ~c U) [~a (~b x)]])", "U", "typ33") (* () *)
      val _ = Check.expect (typ_ "([lam y ((~a I ~b B ~c U) -> (~a B)) ()] [lam x (~a I ~b B) [~a (~b x) ~b (~a x)]])", "U", "typ34") (* () *)
      val _ = Check.expect (typ_ "([lam y ((~a I ~b B ~c U) -> (~a B)) (y [~a 2 ~b T ~c ()])] [lam x (~a I ~b B ~c U) [~a (~b x)]])", "(~a B)", "typ35") (* (~a T) *)
      val _ = Check.expect (typ_ "([lam y ((~a I ~b B ~c U) -> (~a B)) (y [~a 2 ~b T ~c ()])] [lam x (~a I ~b B) [~a (~b x) ~b (~a x)]])", "(~a B)", "typ36") (* (~a T ~b 2) *)

      val _ = Check.exn (fn () => typ_ "[(~a [~a T ~b 1 ~c ()]) + (~b (~b [~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]))]", "badTyp0")
    in
      println "type check tests done"
    end

  fun parseType (concreteType : string) : Type.typ = (case Parse.parse (Scan.scan ("[lam x " ^ concreteType ^ " ()]")) of
                                                        L.Lam (_, tau, _) => tau
                                                      | _ => raise Fail "bug in parseType; this should never happen")
  fun sty () =
    let
      fun chkT t1 t2 = Check.assertT (TypeCheck.subty (parseType t1, parseType t2), t1^"<:"^t2^" (true)")
      fun chkF t1 t2 = Check.assertF (TypeCheck.subty (parseType t1, parseType t2), t1^"<:"^t2^" (false)")
      val _ = chkT "(~a I ~b B)" "(~a I)" (* width subtyping *)
      val _ = chkT "(~a (~x I ~y I) ~b B)" "(~a (~x I) ~b B)" (* depth subtyping ex from class*)
      val _ = chkT "(~x (~a I ~b I) ~y (~m I))" "(~x (~a I))" (* depth subtyping ex from book *)
      val _ = chkT "(~a I ~b B)" "(~b B ~a I)" (* permutation subtyping *)
      val _ = chkT "(~a (~x I ~y I) ~b B ~c I)" "(~b B ~a (~x I))" (* all subtyping ex from ed *)
      val _ = chkF "(~a I)" "(~a I ~b B)"
    in
      println "subtype tests done"
    end

  fun evl_ x = L.tos(Eval.eval(RecordCheck.check(Parse.parse(Scan.scan(x)))))
  fun evl () =
    let
      val _ = Check.expect (evl_ "0", "0", "evl0")
      val _ = Check.expect (evl_ "3", "3", "evl1")
      val _ = Check.expect (evl_ "T", "T", "evl2")
      val _ = Check.expect (evl_ "F", "F", "evl3")
      val _ = Check.expect (evl_ "()", "()", "evl4")
      val _ = Check.expect (evl_ "[2 + 3]", "5", "evl5") (* 5 *)
      val _ = Check.expect (evl_ "[3 - 2]", "1", "evl6") (* 1 *)
      val _ = Check.expect (evl_ "[2 * 3]", "6", "evl7") (* 6 *)
      val _ = Check.expect (evl_ "[20 - [2 + [2 * [2 * 4]]]]", "2", "evl8") (* 2 *)
      val _ = Check.expect (evl_ "[[8 + 4] < [20 - 2]]", "T", "evl19") (* T *)
      val _ = Check.expect (evl_ "!T", "F", "evl10") (* F *)
      val _ = Check.expect (evl_ "!F", "T", "evl11") (* T *)
      val _ = Check.expect (evl_ "[2 == 3]", "F", "evl12") (* F *)
      val _ = Check.expect (evl_ "[2 == 2]", "T", "evl13") (* T *)
      val _ = Check.expect (evl_ "[T ? T : F]", "T", "evl14") (* T *)
      val _ = Check.expect (evl_ "[F ? F : T]", "T", "evl15") (* T *)
      val _ = Check.expect (evl_ "[T ? 0 : 1]", "0", "evl16") (* 0 *)
      val _ = Check.expect (evl_ "[F ? 1 : 0]", "0", "evl17") (* 0 *)
      val _ = Check.expect (evl_ "{x 1 T}", "T", "evl18") (* T *)
      val _ = Check.expect (evl_ "{x 1 x}", "1", "evl19") (* 1 *)
      val _ = Check.expect (evl_ "{x 2 [x * x]}", "4", "evl20") (* 4 *)
      val _ = Check.expect (evl_ "{x 0 {x T !x}}", "F", "evl21") (* F *)
      val _ = Check.expect (evl_ "[~a 1 ~b T ~c ()]", "(~a 1 ~b T ~c ())", "evl22")
      val _ = Check.expect (evl_ "[~a [~x 1 ~y 2] ~b [~x 3 ~y 4]]", "(~a (~x 1 ~y 2) ~b (~x 3 ~y 4))", "evl23")
      val _ = Check.expect (evl_ "[~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]", "(~a (~a 1 ~b 2) ~b (~a 3 ~b 4))", "evl24")
      val _ = Check.expect (evl_ "(~a [~a 1 ~b T ~c ()])", "1", "evl25") (* 1 *)
      val _ = Check.expect (evl_ "(~b [~a [~a 1 ~b 2] ~b [~a 3 ~b 4]])", "(~a 3 ~b 4)", "evl26") (* (~a 3 ~b 4) *)
      val _ = Check.expect (evl_ "(~b (~b [~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]))", "4", "evl27") (* 4 *)
      val _ = Check.expect (evl_ "[(~a [~a 1 ~b T ~c ()]) + (~b (~b [~a [~a 1 ~b 2] ~b [~a 3 ~b 4]]))]", "5", "evl28") (* 5 *)
      val _ = Check.expect (evl_ "[T ? [~a 1 ~b 2] : [~b 3]]", "(~a 1 ~b 2)", "evl29") (* (~a 1 ~b 2)? *)
      val _ = Check.expect (evl_ "([lam x I [x + x]] 2)", "4", "evl30") (* 4 *)
      val _ = Check.expect (evl_ "([lam x (~c I) (~c x)] [~a () ~b F ~c 3])", "3", "evl31") (* 3 *)
      val _ = Check.expect (evl_ "[lam x (~a I ~b B) [~a (~b x) ~b (~a x)]]", "[lam x (~a I ~b B) (~a (~b x) ~b (~a x))]", "evl32") (* ? *)
      val _ = Check.expect (evl_ "[lam x (~a I ~b B ~c U) [~a (~b x)]]", "[lam x (~a I ~b B ~c U) (~a (~b x))]", "evl33") (* ? *)
      val _ = Check.expect (evl_ "([lam y ((~a I ~b B ~c U) -> (~a B)) ()] [lam x (~a I ~b B ~c U) [~a (~b x)]])", "()", "evl34") (* () *)
      val _ = Check.expect (evl_ "([lam y ((~a I ~b B ~c U) -> (~a B)) ()] [lam x (~a I ~b B) [~a (~b x) ~b (~a x)]])", "()", "evl35") (* () *)
      val _ = Check.expect (evl_ "([lam y ((~a I ~b B ~c U) -> (~a B)) (y [~a 2 ~b T ~c ()])] [lam x (~a I ~b B ~c U) [~a (~b x)]])", "(~a T)", "evl36") (* (~a T) *)
      val _ = Check.expect (evl_ "([lam y ((~a I ~b B ~c U) -> (~a B)) (y [~a 2 ~b T ~c ()])] [lam x (~a I ~b B) [~a (~b x) ~b (~a x)]])", "(~a T ~b 2)", "evl37") (* (~a T ~b 2) *)
      val _ = Check.expect (evl_ "([lam n I [[n==0] ? 0 : [n - 1]]] 0)", "0", "evl38")
      val _ = Check.expect (evl_ "([lam n I [[n==0] ? 0 : [n - 1]]] 3)", "2", "evl39")
      val _ = Check.expect (evl_ "((fix [lam sum (I -> I) [lam n I [[n==0] ? 0 : (sum [n - 1])]]]) 0)", "0", "evl40")
      val _ = Check.expect (evl_ "((fix [lam sum (I -> I) [lam n I [[n==0] ? 0 : (sum [n - 1])]]]) 4)", "0", "evl41")
      val _ = Check.expect (evl_ "((fix [lam sum (I -> I) [lam n I [[n==0] ? 0 : [n + (sum [n - 1])]]]]) 0)", "0", "evl42")
      val _ = Check.expect (evl_ "((fix [lam sum (I -> I) [lam n I [[n==0] ? 0 : [n + (sum [n - 1])]]]]) 5)", "15", "evl43")
      val _ = Check.expect (evl_ "((fix [lam fact (I -> I) [lam n I [[n==0] ? 1 : [n * (fact [n - 1])]]]]) 0)", "1", "evl44")
      val _ = Check.expect (evl_ "((fix [lam fact (I -> I) [lam n I [[n==0] ? 1 : [n * (fact [n - 1])]]]]) 5)", "120", "evl45")
    in
      println "eval tests done"
    end

  fun all () =
    let
        val _ = rch ()
        val _ = typ ()
        val _ = sty ()
        val _ = evl ()
    in
      println "all tests done"
    end
      
end
