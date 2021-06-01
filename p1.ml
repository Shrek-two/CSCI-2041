open List


let rec mergesort u = 
    let mergesorting u e  = 
        if List.length u < 2
            then u
        else
            merge e (mergesort (fst (split u)), mergesort (snd (split u)))
    in mergesorting u []
    

and split u = 
    let rec splitting u a = 
        if List.length u < 2
        then (u @ fst a, snd a)
        else
            splitting (tl (tl u)) ((hd u :: fst a), ((hd (tl u)) :: snd a))
    in splitting u ([],[])
    

and merge s a =
    if (fst a) = [] || (snd a) = []
    then s @ fst a @ snd a
    else
        if (hd (fst a)) < (hd (snd a))
        then
            merge (s @ [hd (fst a)]) ((tl (fst a)), snd a)
        else
            merge (s @ [hd (snd a)]) (fst a , (tl (snd a)))
;;



let printThings format things =
  let rec printingThings things =
    match things
    with [] -> () |
         firstThing :: otherThings ->
           Printf.printf " ; " ;
           Printf.printf format firstThing ;
           printingThings otherThings
  in Printf.printf "[" ;
     (match things
      with [] -> () |
           firstThing :: otherThings -> 
             Printf.printf format firstThing ;
             printingThings otherThings) ;
     Printf.printf "]\n" ;;

printThings "%i" (mergesort [5; 2; 6; 42; 14; 62; 4]) ;;  

printThings "%i" (mergesort [1; 54; 35; 23; 7; 23; 866; 2; 5]) ;;

printThings "%i" (mergesort [12; 523; 6;3;2; 76;234; 76; 1]) ;;

(*OUTPUT*)

(* 
[2 ; 4 ; 5 ; 6 ; 14 ; 42 ; 62]
- : unit = ()
[1 ; 2 ; 5 ; 7 ; 23 ; 23 ; 35 ; 54 ; 866]
- : unit = ()
[1 ; 2 ; 3 ; 6 ; 12 ; 76 ; 76 ; 234 ; 523]
- : unit = ()
 *)