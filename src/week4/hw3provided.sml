(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		             | Variable of string
		             | UnitP
		             | ConstP of int
		             | TupleP of pattern list
		             | ConstructorP of string * pattern

datatype valu = Const of int
	            | Unit
	            | Tuple of valu list
	            | Constructor of string * valu

fun g f1 f2 p =
    let 
	      val r = g f1 f2 
    in
	      case p of
	          Wildcard          => f1 ()
	        | Variable x        => f2 x
	        | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	        | ConstructorP(_,p) => r p
	        | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	           | UnitT
	           | IntT
	           | TupleT of typ list
	           | Datatype of string

           (**** you can put all your code here ****)

val only_capitals =
    List.filter (fn word : string => Char.isUpper (String.sub(word, 0)))

val longest_string1 =
    List.foldl (fn (acc: string, elem : string) => if String.size(acc) < String.size(elem) then elem else acc)("")

val longest_string2=
    List.foldl (fn (acc, elem) => if String.size(acc) > String.size(elem) then acc else elem)("")

val longest_string_helper = fn cmp => List.foldl(fn (acc, elem) => if cmp(String.size(elem), String.size(acc)) then elem else acc)("")
                                                
val longest_string3 = longest_string_helper(fn (a : int, b : int) => a > b)
val longest_string4 = longest_string_helper(fn (a : int, b : int) => a >= b)
                                                       

val longest_capitalized = fn words => if only_capitals(words) <> []
                              then
                                 longest_string1(only_capitals(words))
                              else
                                  ""
val rev_string = fn words => implode(rev(explode(words)))

val first_answer = fn func : ('a -> 'b option) => (fn alist : 'a list => let
                                   val mapped = List.map(func)(alist)
                                   val filtered = List.filter(fn a => isSome(a))(mapped)
                               in
                                   if null filtered
                                   then
                                       raise NoAnswer
                                   else
                                       hd filtered
                               end
                              )

val all_answers = fn func => (fn alist => let
                                  val mapped = List.map(func)(alist)
                                  val filtered = List.filter(fn a => isSome(a))(mapped)
                                  val filteredmap = List.map(fn a => valOf a)(filtered)
                              in
                                  List.map(fn a => valOf(a))(filteredmap)
                              end
                                          )

val count_wildcards = fn p => let
                          val curried = g (fn x => 1)(fn y => 0)
                      in
                          curried p
                      end
val count_wild_and_variable_lengths = fn p => let
                                          val curried = g (fn x => 1)(fn y => String.size y)
                                      in
                                          curried p
                                      end
val count_some_var = fn p => let
                         val curried = g (fn x => 0)(fn y => 1)
                     in
                         curried p
                     end

val check_pat = fn p => let
                    val found = List.foldl(fn (a, b) => a @ b) ([]) (p)
                    val boollist = List.foldl (fn (a, b) => if List.exists(fn c => a = c)(b) then [a] else []) ([]) (found)
                in
                    null(boollist)
                end
                            
val match = fn vp => let
                val zipped = ListPair.zip(vp)
                val equals_ = fn (a, b) => let
                                  val zipped = ListPair.zip(a, b)
                                  val mapped = List.map(fn a => if #a = #b then true else false)(zipped)
                                  val reduced = List.foldl(fn (a, b) => a andalso b)(true)(mapped)
                              in
                                  reduced
                              end
                                               
                val helper = fn(v, p) = case p of
                                            Wildcard => SOME ()
                                          | Variable s => SOME (s, v)
                                          | UnitP => (case v of
                                                        Unit => SOME ()
                                                        | _ => NONE)
                                          | ConstP k => (case v of
                                                             Const n => if k = n then SOME () else NONE
                                                           | _ => NONE)
                                          | TupleP ps => (case v of
                                                              Tuple vs => if (null ps andalso null vs) orelse (hd ps) = (hd vs) then
                                                                              let
                                                                                  val res = helper(TupleP (tl ps), Tuple (tl vs))
                                                                              in
                                                                                  if isSome res
                                                                                            (hd vs)::valOf(res)
                                                                                  else
                                                                                      NONE
                                                                              end
                                                          else NONE
                                                         | _ => NONE)
                                                             ConstructorP s1 p => (case v of
                                                                                       Constructor s2 v => if s1 = s2 andalso p = v then
                                                                                                                   SOME p else NONE)
                val found = List.foldl(fn (a, b) => let
                                           val av = #1 a
                                           val ap = #2 a
                                           val bv = #1 b
                                           val bp = #2 b
                                       in
                                           
                                       end
                                      )
            in
            end
