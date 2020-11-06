(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(s : string, lst : string list)  =
    let
        fun help(lst : string list, result : string list, found : bool) =
            case lst of
              [] => (case found of
                    true => SOME result
                  | false => NONE)
              | x::xs => (case same_string(x, s) of
                    false => help(xs, result @ [x], found)
                  | true => help(xs, result, true))
    in
        help(lst, [], false)
    end

(* fun get_substitutions1(llst : string list list, s : string) *)
fun get_substitutions1(lst : string list list, s : string) =
    case lst of
        [] => []
      | x::xs => (case all_except_option(s, x) of
                      SOME found => found @ get_substitutions1(xs, s)
                   |  NONE => get_substitutions1(xs, s))

fun get_substitutions2(lst : string list list, s : string) =
    let
        fun help(lst : string list list) =
            case lst of
                [] => []
              | x::xs => (case all_except_option(s, x) of
                              SOME found => found @ help(xs)
                           |  NONE => help(xs))
    in
        help(lst)
    end

fun similar_names(sub : string list list, tup : {first : string, middle : string, last : string}) =
    let
        val found = get_substitutions1(sub, #first tup);
        fun help(feed : string list) =
            case feed of
                [] => []
              | x::xs => {first=x, middle=(#middle tup), last=(#last tup)}::help(xs)
    in
        {first=(#first tup), middle=(#middle tup), last=(#last tup)}::help(found)
    end
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int;
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw;

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(card : card) =
    case card of
        (Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red

fun card_value(card : card) =
    case card of
        (_, Ace) => 11
     | (_, Num value) => value
     | _ => 10
fun remove_card(cs : card list, c : card, e) =
    let
        fun help(cs : card list, found : bool) =
            case cs of
                [] => (case found of
                           true => []
                        | false => raise e)
              | x::cs => (case x=c of
                              true => help(cs, true)
                            | false => x::help(cs, found))
    in
        help(cs, false)
    end

fun all_same_color(cs : card list) =
    case cs of
        [] => true
     | c1::cs => (case cs of
                       [] => true
                     | c2::_ => (case card_color(c1) = card_color(c2) of
                                    true => all_same_color(cs)
                                 | false => false))

fun sum_cards(cs : card list) =
    case cs of
        [] => 0
     | x::xs => card_value(x) + sum_cards(xs)
fun score(cs : card list, goal : int) =
    let
        val sum = sum_cards(cs)
        fun calc() =
            let
                val gt = sum > goal
            in
                case gt of
                    true => 3 * (sum-goal)
                | false => goal-sum
            end
        val primscore = calc()
        val samescore = primscore div 2
    in
        case all_same_color(cs) of
            true => samescore
         | _ => primscore
    end
        
fun officiate(cs : card list, mvs : move list, goal : int) =
    let
        fun help(held : card list, cs : card list, mvs : move list) =
            case mvs of
                [] => score(held, goal)
              | x::xs => (case x of
                              Discard c => help(remove_card(held, c, IllegalMove), cs, xs)
                            | Draw => (case cs of
                                          [] => score(held, goal)
                                        | c::csx => if sum_cards(c::held) > goal
                                                   then
                                                       score(c::held, goal)
                                                   else
                                                       help(c::held, csx, xs))
                     )
    in
        help([], cs, mvs)
    end
        
