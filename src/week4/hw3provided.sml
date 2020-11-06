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

val first_answer = fn (func : ('a -> 'b option)) => (fn (alist : 'a list) => let
                                   val mapped = List.map(func)(alist)
                                   val filtered = List.filter(fn a => isSome(a))(mapped)
                               in
                                   if null filtered
                                   then
                                       raise NoAnswer
                                   else
                                       valOf(hd filtered)
                               end
                              )

val all_answers = fn func => (fn alist => let
                                  val mapped = List.map(func)(alist)
                                  val filtered = List.filter(fn a => isSome(a))(mapped)
                                  val filteredmap = List.map(fn a => valOf a)(filtered)
                                  val result = List.map(fn a => valOf(a))(filteredmap)
                              in
                                  if null result
                                  then
                                      NONE
                                  else
                                      SOME result
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
fun count_some_var (s, p) = let
                         val curried = g (fn x => 0)(fn y => if y = s then 1 else 0)
                     in
                         curried p
                     end

val check_pat = fn (p : pattern) => let
                    fun help (p) : string list = case (p) of
                                       (Variable x) => [x] 
                                     | (TupleP ps) => let
                                         val psmap = List.map(fn x => help(x)) (ps)
                                     in
                                        List.foldl(fn (a, b) => a @ b) [] psmap
                                     end
                                     | _ => []

                    val found = help (p)
                    val boollist = List.foldl (fn (a, b) => if List.exists(fn c => a = c)(b) then [a] else []) ([]) (found)
                in
                    null(boollist)
                end
(* fun helper p v = case p of
                              Wildcard => SOME []
                            | Variable s => SOME [(s, v)]
                            | UnitP => (case v of
                                            Unit => SOME []
                                          | _ => NONE)
                            | ConstP k => (case v of
                                               Const n => if k = n then SOME [] else NONE
                                             | _ => NONE)
                            | TupleP ps => (case v of
                                                Tuple vs => if (null ps andalso null vs) orelse helper((hd ps), (hd vs)) then
                                                                let
                                                                    val head = helper (hd ps, hd vs)
                                                                    val zipped = ListPair.zip(tl ps, tl vs)
                                                                    val remains = List.map(fn (p1, v1) => helper(p1, v1)) zipped
                                                                    val failed = List.filter(fn a => not (isSome a)) remains
                                                                    val successed = List.filter(fn a => isSome a) remains
                                                                    val mapped = List.map(fn a => valOf a) remains
                                                                in
                                                                    if isSome head andalso null failed
                                                                    then
                                                                        SOME (valOf(head) @ mapped)
                                                                    else
                                                                        NONE
                                                                end
                                                            else NONE
                                              | _ => NONE)
                            | ConstructorP sp => (case v of
                                                        Constructor sv => if #1 sp = #1 sv andalso isSome(helper(#2 sp, #2 sv)) then
                                                                                SOME [(p)] else NONE)
                                              | _ => NONE *)
fun help (v, p): (string * valu) list option = case (p, v) of
                    (Wildcard, _) => SOME []
                  | (Variable s, v) => SOME [(s, v)] 
                  | (UnitP, Unit) => SOME []
                  | (ConstP k, Const n) => if k = n
                                           then
                                               SOME []
                                           else
                                               NONE
                  | (TupleP ps, Tuple vs) => if List.length ps = List.length vs
                                             then
                                                 let
                                                     val mapped = List.map(fn p => help p) (ListPair.zip(vs, ps))
                                                     fun flat xs = List.foldr (fn (x, acc) => x @ acc) [] xs
                                                     val filtered = List.filter(fn a => isSome a) mapped
                                                     val result = List.map(fn a => valOf a) filtered
                                                     val testing = List.filter(fn a => not (isSome a)) mapped
                                                 in
                                                     if null testing
                                                     then
                                                         SOME (flat(result))
                                                     else
                                                         NONE
                                                 end
                                             else
                                                 NONE
                  | (ConstructorP (s1, p1), Constructor (s2, v1)) => if s1 = s2
                                                                     then
                                                                         let
                                                                             val p = help(v1, p1)
                                                                         in
                                                                             p
                                                                         end
                                                                     else
                                                                         NONE
                  | _ => NONE

val match = fn (v, p) => let
                val found = help(v, p)
            in
                if isSome found
                then SOME (valOf(found))
                else NONE
            end
fun first_match v pats =
                        let
                            val found = List.map (fn a => (v, a)) pats
                            val curried = first_answer help
                        in
                            SOME (curried found)
                            handle NoAnswer => NONE
                        end
