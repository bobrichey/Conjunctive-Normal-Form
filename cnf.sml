(* FILENAME: cnf.sml
 * 
 * DESCRIPTION: provides a set of functions for converting a logical sentence 
 * or list of sentences into conjunctive normal form and displaying the results. 
 *
 * The program has been adapted from sample code provided by Dr. Richard Wyatt. 
 *)



Control.Print.printDepth  := 200;        (* set the depth of an object (list) to print *) 
Control.Print.printLength := 200;        (* set the length of an object (list) to print *) 



(* DEFINE THE LOGICAL CONNECTIVES *)
infix -->;  (* NB: "->" is reserved for tycon statements involving functions *) 
infix v;
infix &;
infix <->;

(* DATA TYPE FOR A SENTENCE *) 
datatype sentence =  P | Q | R | S | T             (* allowable sent. vars    *) 
                   | ~ of sentence                 (* negation:      ~P       *) 
                   | v of sentence * sentence      (* disjunction:   P v Q    *) 
                   | & of sentence * sentence      (* conjunction:   P & Q    *) 
                   | --> of sentence * sentence    (* conditional:   P --> Q  *) 
                   | <-> of sentence * sentence;   (* biconditional: P <-> Q  *) 



(* REMOVE ARROWS (CONDITIONALS AND BICONDITIONALS) *) 
fun removeArrows(f <-> g) = removeArrows((f & g) v (~f & ~g))
  | removeArrows(f --> g) = removeArrows(~f v g)
  | removeArrows(f v g)   = removeArrows f v removeArrows g
  | removeArrows(f & g)   = removeArrows f & removeArrows g
  | removeArrows(~f)      = ~(removeArrows f)
  | removeArrows f        = f

(* BRING IN NEGATION, REMOVING DOUBLE NEGATIONS AS WE GO *)
fun bringInNegation(~(~f))    = bringInNegation f
  | bringInNegation(~(f v g)) = bringInNegation(~f & ~g)
  | bringInNegation(~(f & g)) = bringInNegation(~f v ~g)
  | bringInNegation(f v g)    = bringInNegation f v bringInNegation g
  | bringInNegation(f & g)    = bringInNegation f & bringInNegation g
  | bringInNegation f         = f

(* HELP FUNCTION FOR distribDisjInConj *)
fun distribDisjInConj2(f, (g & h)) = distribDisjInConj2(f, g) & distribDisjInConj2(f, h)
  | distribDisjInConj2((g & h), f) = distribDisjInConj2(f, g) & distribDisjInConj2(f, h)
  | distribDisjInConj2(f, g)       = f v g

(* DISTRIBUTE THE DISJUNCTION IN THE CONJUNCTIONS *)
fun distribDisjInConj(f & g) = distribDisjInConj f & distribDisjInConj g 
  | distribDisjInConj(f v g) = distribDisjInConj2(distribDisjInConj f, distribDisjInConj g)
  | distribDisjInConj f      = f

(* PUT SENTENCE INTO CONJUNCTIVE NORMAL FORM *)
fun cnf s = distribDisjInConj(bringInNegation(removeArrows s))



(* HELP FUNCTION FOR SHOW *)
fun show2(f <-> g) = (print "("; show2 f; print "<->"; show2 g; print ")")
  | show2(f --> g) = (print "("; show2 f; print "->"; show2 g; print ")")
  | show2(f & g)   = (print "("; show2 f; print "&"; show2 g; print ")")
  | show2(f v g)   = (print "("; show2 f; print "v"; show2 g; print ")")
  | show2(~f)      = (print "-"; show2 f)
  | show2 P        = print "P"
  | show2 Q        = print "Q"
  | show2 R        = print "R"
  | show2 S        = print "S"
  | show2 T        = print "T"

(* DISPLAY SENTENCE *)
fun show(f <-> g) = (show2 f; print "<->"; show2 g)
  | show(f --> g) = (show2 f; print "->"; show2 g)
  | show(f & g)   = (show2 f; print "&"; show2 g)
  | show(f v g)   = (show2 f; print "v"; show2 g)
  | show(~f)      = (print "-"; show2 f)
  | show f        = show2 f



(* TOP LEVEL FUNCTIONS *)
fun run s  =  (print "\nSentence is: "; 
               show s; 
               print "\n Its CNF is: ";
               show(cnf s); 
               print "\n\n");

fun printNStr(s,0) = ()
  | printNStr(s,n) = (print s; printNStr(s,n-1));

fun go1(_,_,nil) = print "\n"
  | go1(i,n,s::ss) = if i>n 
                         then () 
                     else (print "\n";
                           if i>=10 then printNStr(" ",69) else printNStr(" ",70);
                           print "Example F";
                           print(Int.toString i);
                           run s; 
                           printNStr("=", 80);
                           go1(i+1,n,ss));

(* TOP LEVEL DRIVING FUNCTION *)
fun go s =  let 
                val count = length s
            in
                (printNStr("=",80);
                 go1(1,count,s) )
            end;
