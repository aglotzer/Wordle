
structure Wordle =
struct

open Data
    
datatype result =
    Right
  | Wrong

(* each string is a single capital letter *)
type word = string Seq.seq

(* each string is a single capital letter, paired with a result *)
type guess_result = (string * result) Seq.seq 

(* Checks a guess against a "hidden" word 'answer' and returns the result.
   
   E.g. check(makeWord "STAND", makeWord "STAMP") = 
        <("S",Right), ("T",Right), ("A",Right), ("M",Wrong), ("P",Wrong) >

   Assumes both words have the same length.
*)
fun check(answer : word, guess : word) : guess_result = 
    Seq.map (fn (ans_let, guess_let) => (guess_let,
                                         case ans_let = guess_let of
                                             true => Right
                                           | false => Wrong),
             Seq.zip(answer, guess))

(* little helper to make it easy to try check:
   try running runCheck("STAND","STAMP") in SMLNJ 
   and you should see the above list
*)
fun runCheck(ans : string, guess : string) : (string * result) list =
    Seq.tolist(check (makeWord ans, makeWord guess))
    



(* TASK *)
fun update(possibilities : word Seq.seq, guess : guess_result) : word Seq.seq =
    Seq.filter(fn w => let val (g1, r1) = Seq.nth(0, guess)
                            val (g2, r2) = Seq.nth(1, guess)
                            val (g3, r3) = Seq.nth(2, guess)
                            val (g4, r4) = Seq.nth(3, guess)
                            val (g5, r5) = Seq.nth(4, guess)
                            val comp1 = String.compare(Seq.nth(0, w), g1)
                            val comp2 = String.compare(Seq.nth(1, w), g2)
                            val comp3 = String.compare(Seq.nth(2, w), g3)
                            val comp4 = String.compare(Seq.nth(3, w), g4)
                            val comp5 = String.compare(Seq.nth(4, w), g5)
                            in ((comp1 = EQUAL andalso r1 = Right) orelse ((comp1 = LESS orelse comp1 = GREATER) andalso r1 = Wrong)) andalso
                                ((comp2 = EQUAL andalso r2 = Right) orelse ((comp2 = LESS orelse comp2 = GREATER) andalso r2 = Wrong)) andalso
                                ((comp3 = EQUAL andalso r3 = Right) orelse ((comp3 = LESS orelse comp3 = GREATER) andalso r3 = Wrong)) andalso
                                ((comp4 = EQUAL andalso r4 = Right) orelse ((comp4 = LESS orelse comp4 = GREATER) andalso r4 = Wrong)) andalso
                                ((comp5 = EQUAL andalso r5 = Right) orelse ((comp5 = LESS orelse comp5 = GREATER) andalso r5 = Wrong))
                            end,
                            possibilities)

(*Work of Update: O(n) where n is the length of possibilities
Span of Update: O(1)*)


(* TASK *)
fun suggest(possibilities : word Seq.seq) : word =
    let val freqseq = Seq.map(fn w' => Seq.map(fn l => case l of
                                        "A" => (l, 1)
                                        |"B" => (l, 4)
                                        |"C" => (l, 2)
                                        |"D" => (l, 3)
                                        |"E" => (l, 1)
                                        |"F" => (l, 4)
                                        |"G" => (l, 4)
                                        |"H" => (l, 3)
                                        |"I" => (l, 1)
                                        |"J" => (l, 5)
                                        |"K" => (l, 5)
                                        |"L" => (l, 2)
                                        |"M" => (l, 3)
                                        |"N" => (l, 2)
                                        |"O" => (l, 1)
                                        |"P" => (l, 3)
                                        |"Q" => (l, 5)
                                        |"R" => (l, 1)
                                        |"S" => (l, 2)
                                        |"T" => (l, 2)
                                        |"U" => (l, 3)
                                        |"V" => (l, 5)
                                        |"W" => (l, 4)
                                        |"X" => (l, 5)
                                        |"Y" => (l, 4)
                                        |"Z" => (l, 5)
                                        | _ => (l, 1000) , w'), possibilities)
        val (w, n) = Seq.mapreduce(fn word => let val (l1, v1) = Seq.nth(0, word)
                                                        val (l2, v2) = Seq.nth(1, word)
                                                        val (l3, v3) = Seq.nth(2, word)
                                                        val (l4, v4) = Seq.nth(3, word)
                                                        val (l5, v5) = Seq.nth(4, word)
                                                    in (makeWord(l1^l2^l3^l4^l5), v1+v2+v3+v4+v5)
                                                    end, 
                                                    
                                   (makeWord(""), 1000), 
                                    fn ((w1, n1), (w2, n2)) => (case n1 >= n2 of
                                                                    true => (w2, n2)
                                                                    |false => (w1, n1)),
                                                    freqseq)
            in w
            end

    

fun won(guess : guess_result) : bool =
    Seq.mapreduce(fn (w,r) => case r of Right => true | Wrong => false,
                  true,
                  fn (x,y) => x andalso y,
                  guess)

fun play(answer : word, possibilities : word Seq.seq, shouldPrint : bool) : int =
    let
        val maybePrint = case shouldPrint of true => print | false => fn _ => ()
        
        val guess_word = suggest possibilities
        val () = maybePrint ("Guessing " ^ (String.concat (Seq.tolist guess_word)) ^ "\n")
        val checked = check(answer,guess_word)
    in
        case won checked of
            true => (maybePrint ("You guessed it!\n"); 1)
          | false => play(answer, update(possibilities, checked), shouldPrint) + 1
    end 
        
fun showReal (r : real) = Real.fmt (StringCvt.FIX (SOME 2)) r
    
fun histogram () =
    let val total_games = Seq.length(words) (* Seq.length words *)
        val hist = Dict.toSeq(ExtractCombine.extractcombine (Int.compare, fn w => Seq.singleton(play(w,words, false), 1), Int.+, Seq.take(total_games, words)))
        val ps = Seq.tabulate (fn i => let val (guess,count) = Seq.nth(i,hist)
                                       in
                                           (guess,count, Seq.mapreduce(fn (g,c) => c, 0, fn (x,y) => x + y, Seq.take(i+1, hist)))
                                       end, Seq.length hist)
    in 
    (print "Number of Guesses (N)\t| Games won in N guesses\t| Percentage won in N guesses \t| Percentage won in <=N guesses \n";
     Seq.map(fn (guess,count,cummulative) =>
             print (Int.toString guess ^ "\t\t\t| " ^
                    Int.toString count ^ "\t\t\t\t| " ^
                    showReal (100.0 * real count/real total_games) ^ "\t\t\t\t| " ^ 
                    showReal (100.0 * real cummulative/real total_games) ^ "\n"),
             ps))
    end 

end


(*Work of Extract Combine: O(nlog(n))
Span of Extract Combine: O(n)

Word of Histogram: O(n^2(logn)^2)
Span of Histogram: O(n^2)*)
