(* Write a function called 'count_occurrences' that given a list of chars, returns a list
   of couples containing, for each char, the number of times it appears in the initial list. 
   The final list must be sorted in alphabetic order. 

   Examples: 
   count_occurrences ['c';'i';'a';'o'] -> [('a', 1); ('c', 1); ('i', 1); ('o', 1)]
   count_occurrences ['t';'i';'z';'i';'a';'n';'o'] -> [('a', 1); ('i', 2);
   ('n', 1); ('o', 1);('t', 1); ('z', 1)] *)

let rec tuplify lst = 
  match lst with
    | [] -> []
    | x::xs -> (x,1)::tuplify(xs);;

let rec count_occ l = 
  match l with
    | [] -> []
    | (x,n)::xs -> 
        match xs with
          | [] -> [(x,n)]
          | (y,m)::ys -> if x == y then count_occ((x,n+m)::ys) else (x,n)::count_occ(xs);;

let count_occurrences l = 
  let r1 = List.sort compare l in
  let r2 = tuplify(r1) in 
    count_occ(r2);;

let res = count_occurrences ['t';'i';'z';'i';'a';'n';'o'];;
let res = count_occurrences ['t'; 't'; 't'; 'z'; 't';];;
let res = count_occurrences ['t'; 't'; 't'; 'a';'a'];;
let res = count_occurrences [];;
let res = count_occurrences ['c';'i';'a';'o'];;

(* Scrivere una funzione sub_ord che applicata ad una lista di interi restituisca
   una lista di liste ciascuna delle quali è ordinata (in senso non decrescente) e
   contiene il massimo numero possibile di elementi consecutivi delle lista.

   Esempio:
   sub_ord [4;4;10;20;5;30;6;10] --> [[4;4;10;20];[5;30];[6;10]]
   sub_ord [5;6;4;3;2;1] --> [[5;6];[4];[3];[2];[1]]
*)

let rec sub_ord lst = 
  match lst with
    | [] -> []
    | x::[] -> [[x]]
    | x::xs -> let res = sub_ord xs in 
          if x <= List.hd (List.hd res)
          then (x::(List.hd res)) :: (List.tl res) 
          else [x] :: res
;;

let res = sub_ord([4;4;10;20;5;30;6;10]);;
let res = sub_ord [5;6;4;3;2;1];;
let res = sub_ord([4;4;5;5;10;5;6;10;0]);;


(* Scrivete una funzione flip che scambia gli elementi consecutivi di
   una lista. Se la lista ha un numero dispari di elementi, l’ultimo 
   elemento non viene scambiato.

   Esempio:
   flip [a1;a2;....an] = [a2;a1;a4;a3;a6;a5;....]
*)

let rec flipflop lst = 
  match lst with
    | [] -> []
    | x::xs -> match xs with
      | [] -> []
      | y::ys -> y::x::flipflop(ys);;

let rec reducelist lst = 
  match lst with
    | [] -> []
    | [x] -> []
    | x::xs -> x::reducelist(xs);;


let flip lst = 
  if (List.length lst) mod 2 == 0 
  then flipflop lst 
  else 
    let last_unflipped = List.hd(List.rev lst) 
    and flipped = flipflop lst 
    in flipped @ [last_unflipped]
;;


(* Scrivere una funione PRIMES che dato un numero intero N restituisca la lista dei 
   fattori primi di N con le opportune ripetizioni.

   es: # primes 525;;   --> - : int list = [3; 5; 5; 7]
*)

let rec is_prime number divisor = 
  match number with
    | 1 -> []
    | _ -> if number mod divisor == 0 
        then divisor :: is_prime (number / divisor) divisor
        else is_prime number (divisor+1);;

let primes n = 
  is_prime n 2;;

let res = primes 525;;
let res = primes 12;;


(* Altra soluzione possibile *)

let rec is_prime n d = 
  if d<2 
  then true
  else (if n mod d=0 
        then true 
        else is_prime n (d-1));;

let primo n = 
  is_prime n (n-1);;

let rec cerca_primi n d = 
  if  n = 1 
  then [] 
  else (if n mod d=0 && primo d
        then d::cerca_primi (n/d) d
        else cerca_primi n (d+1));;

let primes n = 
  cerca_primi n 2;;

let res = primes 525;;
let res = primes 12;;


(* Scrivete una funzione extend che dato un elemento x ed una lista di liste l 
   aggiunge x in testa a ciascuna delle liste di l. 

   Esempio: extend 1 [[2;3]; [4;5;6]] = [[1;2;3]; [1;4;5]]
*)


let rec extend x l = 
  match l with
    | [] -> []
    | y::ys -> (x :: y) :: extend x ys;;

let res = extend 1 [[2;3]; [4;5;6]];;

(* Scrivere una funzione che rimuova il secondo elemento di una lista di 
   interi. Se la lista ha un solo elemento deve ritornare quell’elemento e
   se la lista è vuota deve ritornare la lista vuota.
*)

let remove_second lst = 
  match lst with
    | [] -> []
    | [x] -> [x]
    | x::y::xs -> x::xs;;


let res = remove_second [1;2;3];;
let res = remove_second [1;2];;
let res = remove_second [1];;
let res = remove_second [];;


(* Write a function called 'count_odd_digits' that, given an integer, returns the 
   number of odd numbers in the decimal representation of that integer. 

   Examples:
   count_odd_digits 12896  = 2
   count_odd_digits 1279  = 3
*)


let rec count_odd_digits number =
  let acc = 0 in 
    match number with
      | 0 -> acc
      | _ -> if (number mod 10) mod 2 == 0 
          then acc + count_odd_digits (number / 10) 
          else acc + 1 + count_odd_digits (number / 10);;

let res = count_odd_digits 12896;;
let res = count_odd_digits 1279;;

(*  Scrivere una funzione sum per calcolare la somma di due numeri codificati con liste di cifre.
    La funzione ritorner`a la somma sottoforma di lista di cifre. 

    Esempio: 
    sum [1;2;3] [4;0;9] = [5;3;2]. 

    Note bene: non è possibile convertire le liste di cifre nei numeri che queste
    rappresentano per poi farne la somma, ma biogna simulare un’operazione di somma cifra per
    cifra con eventuali riporti.
*)

let rec basic_sum l1 l2 = 
  match l1,l2 with
    | [], _ -> l2
    | _, [] -> l1
    | _, _ -> [List.hd l1 + List.hd l2] @ basic_sum (List.tl l1) (List.tl l2);;

let rec adjust_sum lst = 
  match lst with
    | [] -> []
    | x::[] -> if x > 9 then (x mod 10) :: [(x / 10)] else x::[]
    | x::xs -> let res = adjust_sum xs in 
          if x > 9 then (x mod 10)::(List.hd res + (x / 10))::(List.tl res) else x::res;;

let sum l1 l2 = 
  List.rev (adjust_sum (List.rev (basic_sum l1 l2))));;

let res2 = sum [1;2;3] [4;0;9];; (*Restituisce: [5;3;2]*)
let res2 = sum [9;5] [5;9];; (*Restituisce: [1; 5; 4]*)



(********************************************************************************************)
(*              Programmazione part-time - Parte pratica dell'esame del 2012                *)


(* Scrivere una funzione che data una lista di caratteri restituisca il numero di caratteri 
   uguali ad 'e' oppure 'a'. *)

let rec count_ae lst = 
  let acc = 0 in 
    match lst with
      | [] -> acc
      | x::xs -> if x == 'a' || x == 'e' then acc + 1 + count_ae xs else acc + count_ae xs;;

let res = count_ae ['a';'b';'a';'b';'e'];; 
let res = count_ae ['b';'c';'d'];; 

(* Scrivere una funzione che data una lista di caratteri ritorni una nuova lista con i
   caratteri e ed a rimpiazzati da x. *)

let rec replace_ae lst = 
  match lst with
    | [] -> []
    | x::xs -> if x == 'a' || x == 'e' then 'x' :: replace_ae xs else x::replace_ae xs;;

let res = replace_ae ['a';'b';'a';'b';'e'];; 
let res = replace_ae ['b';'c';'d'];; 


(******************************************************************************************)
(*              Programmazione mod. 1 - Parte pratica dell'esame del 2012                 *)

(* Definire una funzione nth che dati:
   - una lista di lunghezza n > 0,
   - un intero k compreso tra 0 e n-1, 
   restituisca il k-esimo elemento della lista, assumendo che gli elementi della lista
   siano numerati da 0 a n-1. *)

let rec nth lst k = 
  match k with
    | n when n < 0 || k > List.length lst -> failwith "Errore: k dev'essere entro i limiti"
    | _ -> List.nth lst k;;


let res = nth [1;2;3] 2;;
let res = nth [1;2;3] (-1);;
let res = nth [1;2;3] (100);;

(*****************************************************************************************
   Dato un numero intero n, definire una funzione che calcola la sommatoria da 1 a n
*)

let rec sommatoria n = 
  match n with
    | k when k < 0 -> failwith "Error: input is not an integer"
    | k when k = 0 -> 0
    | _ -> n+sommatoria(n-1);;

let res = sommatoria 5;;
let res = sommatoria 0;;


(*****************************************************************************************
   Dato un numero intero n, definire una funzione che calcola il fattoriale di n
*)

let rec fattoriale n = 
  match n with
    | k when k < 0 -> failwith "Error: input is not an integer"
    | k when k = 0 -> 0
    | k when k = 1 -> 1
    | _ -> n*fattoriale(n-1);;

let res = fattoriale 5;;
let res = fattoriale 0;;


(*******************************************************************************
   Definire una funzione che data una lista ritorni una coppia i cui elementi sono il primo e l’ultimo elemento della lista. Se la lista `e vuota lanciare un’eccezione. Se la lista ha un solo elemento, la funzione deve restituire il primo e l'ultimo elemento.
*)

let rec last_element lst = 
  match lst with
    | [] -> failwith "Error: empty string"
    | x::[] -> x
    | x::xs -> last_element xs;;

let res = last_element [3;1;2;4;5];;

let primo_ultimo lst = 
  match lst with
    | [] -> failwith "Error: empty string"
    | x::[] -> (x,x)
    | x::xs -> (x, last_element(xs));;

let res = primo_ultimo [3;1;2;4;5];;
let res = primo_ultimo [];;
let res = primo_ultimo [1];;

(******************************************************************************
   Given a list of chars, write a function that returns the number of 'a' or 'e' chars in the list. *)

let rec isAorE lst =
  match lst with
    | [] -> 0
    | x::xs -> 
        if x == 'a' || x == 'e' 
        then 1+isAorE(xs) 
        else isAorE(xs);;

let res = isAorE ['t';'i';'z';'i';'a';'n';'o'];;
let res = isAorE ['a'; 'e'; 'a'; 'z'; 't';];;

(******************************************************************************
   Given a list of chars, write a function that returns a new list where chars a and i are replaced with x. *)


let rec replaceAorE lst =
  match lst with
    | [] -> []
    | x::xs -> 
        if x == 'a' || x == 'e' 
        then 'x'::replaceAorE(xs) 
        else x::replaceAorE(xs);;

let res = replaceAorE ['t';'i';'z';'i';'a';'n';'o'];;
let res = replaceAorE ['a'; 'e'; 'a'; 'z'; 't';];;

(******************************* 99 Problems **********************************)

(* Function last that returns the last element of a list. *)

let rec last lst = 
  match lst with
    | [] -> failwith "Error: empty string"
    | x::[] -> x
    | x::xs -> last_element xs;;

let res = last [3;1;2;4;5];;


