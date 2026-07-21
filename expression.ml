(**************************************************************************)
(*                                                                        *)
(*                               GlSurf                                   *)
(*                                                                        *)
(*                   C. Raffalli, Universite de Savoie                    *)
(*                                                                        *)
(* Copyright 2003, 2004 Christohe Raffalli                                *)
(*                                                                        *)
(*  This file is part of GlSurf.                                          *)
(*                                                                        *)
(*  GlSurf is free software; you can redistribute it and/or modify        *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; either version 2 of the License, or     *)
(*  (at your option) any later version.                                   *)
(*                                                                        *)
(*  GlSurf is distributed in the hope that it will be useful,             *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with GlSurf; if not, write to the Free Software                 *)
(*  Foundation, Inc., 59 Temple Place, Suite 330, Boston,                 *)
(*  MA  02111-1307  USA                                                   *)
(**************************************************************************)

open Algebra
open Format

module String = struct
  type t = string
  let compare = compare
end

module StringMap = Map.Make(String)

module type Expression  =
sig
  (* include Field *)
  type scalar

  type trans = (* transantes function, like sin() or cos() *)
      {
	name : string;
	calcul : scalar -> scalar;
	deriv : elem;
	name_var: string
      }

  and trans2 = (* transantes function, like sin() or cos() *)
      {
	name2 : string;
	calcul2 : scalar -> scalar -> scalar;
	deriv1 : elem;
	deriv2 : elem;
	name_var1: string;
	name_var2: string
      }

  and user_fun  = (* user defined functions *)
      {
	fname : string;
        variables : string list;
        value : elem
      }

  and elem =
      Add of elem  * elem
    | Sub of elem  * elem
    | Div of elem  * elem
    | Mul of elem  * elem
    | Pow of elem  * int
    | Tra of int * elem
    | Tra2 of int * elem * elem
    | Cst of scalar
    | Var of string

  type 'a environment = (string * 'a) list

(*** field struct ***)
  val zero : elem              (* zero ! *)
  val one :  elem              (* one ! *)
  val t_of_int : int -> elem   (* the standard mapping from integer *)
  val (++) : elem -> elem -> elem
  val (--) : elem -> elem -> elem
  val ( ** ) : elem -> elem -> elem
  val ( @ ) : scalar -> elem -> elem
  val (==) : elem -> elem -> bool
  val opp : elem -> elem
  val inv : elem -> elem
  val (//) : elem -> elem -> elem
  val cst : scalar -> elem
  val var : string -> elem

  val trans_table : int StringMap.t ref
  val trans_array : trans array ref
  val trans2_table : int StringMap.t ref
  val trans2_array : trans2 array ref
  val fun_table : user_fun StringMap.t ref
  val loc_var : string list ref

  val print : elem -> unit
  val write : formatter -> elem -> unit
  val write_to_string : elem -> string

  val parse : elem Pacomb.Grammar.t
  val write_bin : out_channel -> elem -> unit
  val read_bin : in_channel -> elem

  val simplify : elem -> elem
  val develop : elem -> elem
  val normalize : elem -> elem
  val derive   : elem -> string -> elem
  val laplacien : elem -> string list -> elem
  val sder     : elem -> string -> int -> elem
  val eval     : elem -> scalar environment -> scalar
  val subst    : elem -> elem environment -> elem
  val subst'    : elem -> scalar environment -> elem
  val conjugate : elem -> elem
  val varlist : elem -> string list

  val decompose_poly : elem -> (scalar * (string * int) list) list
  val make_poly : elem -> elem
end

module Make =functor (R:Field) ->
struct
  type scalar = R.elem

  type trans = (* transantes function, like sin() or cos() *)
      {
	name : string;
	calcul : scalar -> scalar;
	deriv : elem;
	name_var: string
      }

  and trans2 = (* transantes function, like sin() or cos() *)
      {
	name2 : string;
	calcul2 : scalar -> scalar -> scalar;
	deriv1 : elem;
	deriv2 : elem;
	name_var1: string;
	name_var2: string
      }

  and user_fun  = (* defined function *)
      {
	fname : string;
        variables : string list;
        value : elem
      }

  and elem =
      Add of elem  * elem
    | Sub of elem  * elem
    | Div of elem  * elem
    | Mul of elem  * elem
    | Pow of elem  * int
    | Tra of int * elem
    | Tra2 of int * elem * elem
    | Cst of scalar
    | Var of string

  type 'a environment = (string * 'a) list

  type  expS = (* simplify type*)
      Som of (scalar * expS) list
    | Pro of (int * expS) list
    | Tra' of int * expS
    | Tra2' of int * expS * expS
    | Var' of string
    | Cst' of scalar

  let trans_table = ref StringMap.empty

  let trans_array = ref [| |]

  let trans2_table = ref StringMap.empty

  let trans2_array = ref [| |]

  let fun_table = ref StringMap.empty

  let rec eval_power exp puis =
    (* power function in field (time log_2(n))*)
    match puis with
      0 -> R.one
    | 1 -> exp
    | _ when puis < 0 -> R.inv(eval_power exp (-puis))
    | _ when puis mod 2 = 0 ->
	let r = eval_power exp (puis / 2) in
	R.( ** ) r r
    | _ ->
	let r = eval_power exp ((puis - 1) / 2) in
 	R.( ** ) exp (R.( ** ) r r)

  (********** output input functions ***********)

  (* la fonction d'affichage, utilisant les priorités usuelles pour les opérateurs binaires *)

  let is_var e = match e with Var _ -> true | _ -> false

  let write ?(lang=Human) formatter expr =

    let print_string s =
      pp_print_string formatter s
    in
    let print_string' s =
      pp_print_space formatter ();
      pp_print_string formatter s;
      pp_print_space formatter ()
    in

    (* dans cette fonctions auxilliaire, le paramètre "l" représente le niveau de priorité courant *)

    let rec fn l e = match e with
	Var name -> print_string name
      | Cst n -> R.write formatter n
      | Add _ | Sub _ as e -> pp l 1 (fns 1) e
      | Mul _ | Div _ as e -> pp l 2 (fnp 2) e
      | Pow (e,n) ->
         begin
           match lang with
           | C ->
	      print_string "powf("; pp l 3 (fn 3) e; print_string ",";
              pp_print_int formatter n; print_string ")"
           | Glsl ->
	      print_string "powf("; pp l 3 (fn 3) e; print_string ",";
              pp_print_int formatter n; print_string ")"
           | Human ->
	      pp l 3 (fn 0) e; print_string "^";
              pp_print_int formatter n;

         end
      | Tra (trindex,e) ->
	 print_string !trans_array.(trindex).name;
	 print_string "(";
         fn 0 e; print_string ")"
      | Tra2 (trindex,e,e') ->
	 print_string !trans2_array.(trindex).name2;
	 print_string "(";
         fn 0 e; print_string ",";
	 fn 0 e'; print_string ")"
	(* affichage associatif des sommes *)
    and fns l e = match e with
      Add(e,e') ->
       fns l e; print_string' "+"; fns l e'
    | Sub(e,e') ->
            fns l e; print_string' "-"; fn 2 e'
    | e -> fn l e

	(* affichage associatif des produits *)
    and fnp l e = match e with
      Mul(e,e') ->
	    fnp l e; print_string' "*"; fnp l e'

    | Div(e,e') ->
            fnp l e; print_string' "/"; fn 3 e'
    | e -> fn l e

	(* fonctions testant les priorités pour afficher ou non les paranthèses *)
    and pp l l' f e =
      if l > l' then print_string "(";
      pp_open_box formatter 2;
      f e;
      pp_close_box formatter ();
      if l > l' then print_string ")"
    in
    pp_open_box formatter 2;
    fn 0 expr;
    pp_close_box formatter ()


  let write_bin ch x = output_value ch x
  let read_bin ch = input_value ch
  let print = write std_formatter

  let write_to_string ?(lang=Human) x =
    let b = Buffer.create 80 in
    let formatter =  formatter_of_buffer b in
    write ~lang formatter x;
    pp_print_flush formatter ();
    Buffer.contents b

  let common es =
    let tbl = Hashtbl.create 101 in
    let rec fn e =
      match e with
      | Var _ -> ()
      | Cst _ -> ()
      | Pow(e,0) -> ()
      | Pow(e,1) -> fn e
      | Pow(e,n) when n < 1 ->
         fn (Div(Cst(R.one), Pow(e,-n)))
      | Pow(e, n) when n mod 2 = 0 ->
         let f = Pow(e,n/2) in
         fn (Mul(f, f))
      | Pow(e, n) ->
         let f = Pow(e,n/2) in
         fn (Mul(Mul(f, f), e))
      | _ ->
         let count, new_ =
           try
             fst (Hashtbl.find tbl e), false
           with Not_found ->
                 let r = ref 0 in
                 Hashtbl.add tbl e (r, ref None);
                 r, true
         in
         incr count;
         if new_ then
           match e with
           | Add(e1,e2) | Sub(e1,e2) | Mul(e1,e2) | Div(e1,e2)
             | Tra2(_,e1,e2) ->
              fn e1; fn e2
           | Tra(_,e) -> fn e
           | _ -> assert false
    in
    List.iter fn es;
    Hashtbl.filter_map_inplace
      (fun _ c -> if !(fst c) <= 1 then None else Some c)
      tbl;
    tbl

  let defs ?(base="def_") es =
    let tbl = common (List.map snd es) in
    let i = ref 0 in
    let res = ref [] in
    let rec fn ?name e =
      try
        let (_, r) = Hashtbl.find tbl e in
        match !r with
        | None ->
           let name =
             match name with
             | Some n -> n
             | None -> base ^ string_of_int !i
           in
           incr i;
           let c = (name, gn true e) in
           r:= Some c;
           res := c :: !res;
           c
        | Some ((name', _) as c) ->
           match name with
           | Some name when name' <> name ->
              let c = (name, Var name') in
              res := c :: !res;
              c
           | _ -> c
      with Not_found ->
        match name with
        | None -> assert false
        | Some n ->
           let c = (n, gn true e) in
           res := c :: !res;
           c

    and gn top e =
      if not top && Hashtbl.mem tbl e then
        let name = fst (fn e) in
        Var name
      else
        let gn = gn false in
        match e with
        | Var _ | Cst _ -> e
        | Add(e1,e2) -> Add(gn e1, gn e2)
        | Sub(e1,e2) -> Sub(gn e1, gn e2)
        | Mul(e1,e2) -> Mul(gn e1, gn e2)
        | Div(e1,e2) -> Div(gn e1, gn e2)
        | Tra(i, e1) -> Tra(i, gn e1)
        | Tra2(i, e1, e2) -> Tra2(i, gn e1, gn e2)
        | Pow(e, 0) -> Cst R.one
        | Pow(e1, 1) -> gn e1
        | Pow(e, n) when n < 0 -> gn (Div(Cst R.one, Pow(e,-n)))
        | Pow(e, n) when n mod 2 = 0 ->
           let f = Pow(e,n/2) in
           gn (Mul(f, f))
        | Pow(e, n) ->
           let f = Pow(e,n/2) in
           gn (Mul(Mul(f, f), e))
    in
    List.iter (fun (name, e) -> ignore (fn ~name e)) es;
    List.rev !res

  let write_defs ?(lang=Human) fmt es =
    let defs = defs es in
    List.iter (fun (name, e) ->
        Format.fprintf fmt "float %s = %a;\n%!" name
          (write ~lang) e) defs

  (******** Simplification procedure *******)

  let rec totypeS e =
    match e with
      Cst n -> Cst' n
    | Var var -> Var' var
    | Tra(tra,exp')->Tra'(tra,totypeS exp')
    | Tra2(tra,exp',exp'')->Tra2'(tra,totypeS exp', totypeS exp'')
    | Pow(exp,puis)->Pro([puis,totypeS exp])
	(* (R.one,cte R.one est la pour que l'expression ait deux arguments *)
    | Mul(e1,e2) -> Pro([1,totypeS e1 ; 1,totypeS e2])
    | Div(e1,e2) -> Pro([1,totypeS e1 ; -1,totypeS e2])
    | Add(e1,e2) -> Som([R.one,totypeS e1 ; R.one,totypeS e2])
    | Sub(e1,e2) -> Som([R.one,totypeS e1 ; R.opp(R.one),totypeS e2])

  let rec totypeE e =
    match e with
      Cst' n -> Cst n
    | Var' var-> Var var
    | Tra'(tra, exp')->Tra(tra,totypeE exp')
    | Tra2'(tra, exp',exp'')->Tra2(tra,totypeE exp',totypeE exp'')
    | Pro([]) -> Cst R.one
    | Pro(x::l)->
	let fn y = match y with
	    (1,exp)-> totypeE exp
	  | (_,exp) when exp=Cst' R.one -> Cst R.one
	  | (n,exp)-> Pow(totypeE exp,n) in
	let f exp arg = Mul(exp, fn arg) in
	List.fold_left f (fn x) l

    | Som([]) -> Cst R.zero
    | Som(x::l)->
	let fn b y = match y with
	    (a,exp) when R.(==) a R.one -> true, totypeE exp
	  | (a,exp) when b && R.(==) a (R.opp R.one) -> false, totypeE exp
	  | (n,Cst' a) -> true, Cst( R.( ** ) n a )
	  | (n,exp)-> true, Mul(Cst n, totypeE exp) in
	let f exp arg =
	  let add, e = fn true arg in
	  if add then Add(exp, e) else Sub(exp, e)
	in

	List.fold_left f (snd (fn false x)) l

  (* ordre sur les termes, utilise pour orienter les sommes et les produits *)
  let prior = function
      Cst' _ -> 0
    | Var' _ -> 1
    | Som  _ -> 2
    | Pro  _ -> 3
    | Tra' _ -> 4
    | Tra2' _ -> 5


  let rec cmp_list l l' fn = match l, l' with
      [], [] -> 0
    | [], _::_ -> -1
    | _::_, [] -> 1
    | (a,x)::l, (a',x')::l' ->
	match fn x x' with
	  0 ->
	    begin
	      match compare a a' with
		0 -> cmp_list l l' fn
	      | c -> c
	    end
	| c -> c

  let rec cmp' x y = match x,y with
      Cst' n, Cst' n' -> compare n n'
    | Var' name, Var' name' -> compare name name'
    | Som (l), Som(l') -> cmp_list l l' cmp'
    | Pro (l), Pro(l') -> cmp_list l l' cmp'
    | Tra'(i,l),Tra'(i',l') ->
	begin
	  match compare i i' with
	    0 -> cmp' l l'
	  | c -> c
	end
    | Tra2'(i,l,m),Tra2'(i',l',m') ->
	begin
	  match compare i i' with
	    0 ->
	      begin
		match cmp' l l' with
		  0 -> cmp' m m'
		| c -> c
	      end
	  | c -> c
	end
    | l1, l2 -> compare (prior l1) (prior l2)

  let cmp_snd x y = - cmp'(snd x)(snd y)

  let rec regroupSom' acc list  =
    match list with
      (a,_)::l when R.(==) a R.zero  -> regroupSom' acc l
    | (_,Cst' n)::l when R.(==) n R.zero   -> regroupSom' acc l
    | (c,x)::(c',x')::l when cmp' x x' = 0 ->
	regroupSom' acc ((R.(++) c c', x)::l)
    | (c,x)::l -> regroupSom' ((c,x)::acc) l
    | [] -> acc

  let regroupSom l =
    let l = List.sort cmp_snd l in
    regroupSom' [] l

  let rec regroupPro' acc list  =
    match list with
      (a,_)::l when a = 0  -> regroupPro' acc l
    | (_,Cst' n)::l when n= R.one   -> regroupPro' acc l
    | (_,Cst' n)::_ when n= R.zero   -> [1,Cst' R.zero]
	(* cas on ne peut pas tout passer dans le premier membre comme pour l'add *)
    | (c,Cst' n)::(c',Cst' n')::l ->
	regroupPro' acc ((1,Cst' (R.( ** )
			       (eval_power n c)
			       (eval_power n' c')
			    ))::l)
    | (c,x)::(c',x')::l when (cmp' x x') = 0 ->
	regroupPro' acc (( c + c', x)::l)
    | (c, Cst' n)::l -> regroupPro' ((1,Cst' (eval_power n c))::acc) l
    | (c,x)::l -> regroupPro' ((c,x)::acc) l
    | [] -> acc

  let regroupPro l =
    let l = List.sort cmp_snd l in
    regroupPro' [] l

  let flat_map f l =
    let rec fn acc = function
	[] -> acc
      | x::l -> fn (List.rev_append (f x) acc) l
    in fn [] l

  let flattenPro l =
    let rec fn acc = function
      (c,Pro l) ->
	(List.rev_map (fun (c', x) -> (c * c', x)) l) @ acc
    | (c, Som []) ->
	[c, Cst' R.zero]
    | (c, Som [c', e]) ->
	fn ((c, Cst' c')::acc) (c,e)
    | (c, x) ->
	(c, x)::acc
  in
  let l = flat_map (fn []) l in
  regroupPro l

  let flattenSom l =
    let rec fn = function
	(c,Som l) ->
	  List.rev_map (fun (c', x) -> (R.( ** ) c c', x)) l
      | (c, Pro []) ->
	  [c, Cst' R.one]
      | (c, Pro([1,e])) ->
	  fn (c, e)
      | (c, Pro([a,Cst' n])) ->
	  [R.( ** ) c (eval_power n a), Cst' R.one]
      | (c, Pro((a,Cst' n)::l1)) ->
	  fn (R.( ** ) c (eval_power n a), Pro l1)
      | (c, x) ->
	  [c, x]
    in
    let l = flat_map fn l in
    let fncst = function
	(c,Cst' n) -> (R.( ** ) c n , Cst' R.one)
      | (c,a) -> (c,a) in
    let l = List.rev_map fncst l in
    regroupSom l

  let rec simplS expS = (*simplification fonction with type expS*)
    let simplSND l = (fst l,simplS (snd l)) in
    let r = match expS with
      Cst' n -> Cst' n
    | Var' name  -> Var' name
    | Tra'(trindex,exp) -> (
	match simplS exp with
	  Cst' n -> Cst' (!trans_array.(trindex).calcul n)
	| e	-> Tra'(trindex, e))
    | Tra2'(trindex,exp,exp') -> (
	match simplS exp, simplS exp' with
	  Cst' n, Cst' m -> Cst' (!trans2_array.(trindex).calcul2 n m)
	| e, e'	-> Tra2'(trindex, e, e'))
    | Pro(l) ->
	let l = flattenPro(List.map simplSND l) in
	begin
	  match l with
	    [] -> Cst' R.one
	  | [1,x] -> x
	  | [a,Cst' n] -> Cst' (eval_power n a)
	  | l -> Pro(l)
	end
    | Som(l) ->
	let l = flattenSom(List.map simplSND l) in
	begin
	  match l with
	    [] -> Cst' R.zero
	  | [c,x] when c = R.one -> x
	  | [c, Cst' x] -> Cst' (R.( ** ) c x)
	  | l -> Som(l)
	end
    in
(*
    print_string "in "; print (totypeE expS); print_newline ();
    print_string "out "; print (totypeE r); print_newline ();
*)
    r

  let simplify exp =
    totypeE (simplS (totypeS exp))

  let distribPro l1 l2 =
    let l = flat_map (fun (c1,e1) ->
	  List.rev_map (fun (c2,e2) ->
	    R.( ** ) c1 c2,
	    Pro(flattenPro [1, e1; 1, e2])) l2) l1 in
    flattenSom l

  let rec developPow list n =
    match n with
      0 ->
	[R.one, Cst' R.one]
    | 1 ->
	list
    | n when n < 0 -> failwith "not implemented"
    | n when n mod 2 = 0 ->
	let list' = developPow list (n / 2) in
	distribPro list' list'
    | n ->
	let list' = developPow list (n / 2) in
	distribPro list (distribPro list' list')

  let rec developPro list =
    match list with
      [] -> [R.one, Cst' R.one]
    | (c, Som s)::l -> distribPro (developPow s c) (developPro l)
    | (c, Pro l')::l ->
	developPro ((List.map (fun (c', x) -> (c * c', x)) l')@l)
    | (c, e)::l -> distribPro [R.one, Pro[c,e]] (developPro l)

  let rec developS a = function
      Pro l -> Som (
	List.map (fun (c,e) -> R.( ** ) a c, e)
	  (developPro (List.map (fun (n,e) -> n, developS R.one e) l)))
    | Som l ->
	let l = List.map (fun (c,e) -> R.one, developS (R.( ** ) a c) e) l in
	Som(flattenSom l)
    | Tra'(f,e) ->
	let e = Tra'(f,developS R.one e) in
	if R.(==) a R.one then e else Som [a,e]
    | Tra2'(f,e,e') ->
	let e = Tra2'(f,developS R.one e, developS R.one e') in
	if R.(==) a R.one then e else Som [a,e]
    | e ->
	if R.(==) a R.one then e else Som [a,e]

  let develop exp =
    totypeE (developS R.one (simplS (totypeS exp)))

  let normalize = simplify

      (************ field's definition ***************)

  let zero = Cst R.zero
  let one  = Cst R.one
  let t_of_int n = Cst (R.t_of_int n)
  let (++)  exp1 exp2 = Add(exp1,exp2)
  let (--)  exp1 exp2 = Sub(exp1,exp2)
  let (//) exp1 exp2 = Div(exp1,exp2)
  let ( ** ) exp1 exp2 = Mul(exp1,exp2)
  let (@) x e = Mul(Cst x, e)
  let opp exp= Sub(Cst R.zero,exp)
  let inv exp =Div(Cst R.one, exp)
  let cst c = Cst c
  let var s = Var s
  let trans f e = Tra(f, e)


  let (==) exp1 exp2 =
    let rec fn x y = match x,y with
	Cst n, Cst n' -> R.(==) n n' (* compare with lexicographic ordre *)
      | Var name, Var name' -> name = name'
      | Add(e1,e2), Add(e1',e2') -> fn e1 e1' && fn e2 e2'
      | Sub(e1,e2), Sub(e1',e2') -> fn e1 e1' && fn e2 e2'
      | Mul(e1,e2), Mul(e1',e2') -> fn e1 e1' && fn e2 e2'
      | Div(e1,e2), Div(e1',e2') -> fn e1 e1' && fn e2 e2'
      | Pow (e,n),  Pow (e',n') ->  fn e e' && n = n'
      | Tra (i,e), Tra (i',e') -> i = i' && fn e e'
      | Tra2 (i,e,f), Tra2 (i',e',f') -> i = i' && fn e e' && fn f f'
      | _ -> false
    in fn (simplify exp1) (simplify exp2)

  let eval expr v = (* eval expression in environnent *)
    let tbl = Hashtbl.create 1001 in
    let rec fn e =
      try Hashtbl.find tbl e with Not_found ->
      let r = match e with
	Var name ->
	  (try
	    List.assoc name v
	  with Not_found ->
	    failwith ("Unbound variable: "^name))
      | Cst n -> n
      | Add (e,e') -> R.(++) (fn e) (fn e')
      | Sub (e,e') -> R.(--) (fn e) (fn e')
      | Mul (e,e') -> R.( ** ) (fn e) (fn e')
      | Div (e,e') -> R.( // ) (fn e) (fn e')
      | Pow (e,deg) -> eval_power (fn e) deg
      | Tra (trindex, e) -> !trans_array.(trindex).calcul (fn e)
      | Tra2 (trindex, e, e') -> !trans2_array.(trindex).calcul2 (fn e) (fn e')
      in Hashtbl.add tbl e r ;
      r
    in fn expr

(* substitution "name" var in "exp1" with "exp2"
   substitution d'une variable "name" par une expression "exp2"
   dans la formule "exp1" *)
  let subst exp1 v =
    let rec fn = function
	Var name as e -> (try List.assoc name v with Not_found -> e)
      | Cst _ as e -> e
      | Add (e,e') -> Add (fn e, fn e')
      | Sub (e,e') -> Sub (fn e, fn e')
      | Mul (e,e') -> Mul (fn e, fn e')
      | Div (e,e') -> Div (fn e, fn e')
      | Pow (e,n) ->  Pow (fn e, n)
      | Tra (tran,e) -> Tra (tran, fn e)
      | Tra2 (tran,e,e') -> Tra2 (tran, fn e, fn e')
    in fn exp1

  let subst' exp1 v =
    let rec fn = function
	Var name as e -> (try Cst (List.assoc name v) with Not_found -> e)
      | Cst _ as e -> e
      | Add (e,e') -> Add (fn e, fn e')
      | Sub (e,e') -> Sub (fn e, fn e')
      | Mul (e,e') -> Mul (fn e, fn e')
      | Div (e,e') -> Div (fn e, fn e')
      | Pow (e,n) ->  Pow (fn e, n)
      | Tra (tran,e) -> Tra (tran, fn e)
      | Tra2 (tran,e,e') -> Tra2 (tran, fn e, fn e')
    in fn exp1

  let conjugate exp1 =
    let rec fn = function
	Var _ as e -> e
      | Cst (v) -> Cst (R.conjugate v)
      | Add (e,e') -> Add (fn e, fn e')
      | Sub (e,e') -> Sub (fn e, fn e')
      | Mul (e,e') -> Mul (fn e, fn e')
      | Div (e,e') -> Div (fn e, fn e')
      | Pow (e,n) ->  Pow (fn e, n)
      | Tra (tran,e) -> Tra (tran, fn e)
      | Tra2 (tran,e,e') -> Tra2 (tran, fn e, fn e')
    in fn exp1


  let varlist expr =
    let rec fn acc = function
	Var name -> if List.memq name acc then acc else name::acc
      | Add (e,e') | Sub (e,e') | Mul (e,e') | Div (e,e') | Tra2(_,e,e') -> fn (fn acc e) e'
      | Pow (e,_) | Tra(_,e) -> fn acc e
      | _ -> acc
    in
    fn [] expr

      (* derivation par rapport a la varible name *)

  let derive expr name =
    let add(e, e') =
      if e = zero then e' else
      if e' = zero then e else
      Add(e,e')
    in
    let sub(e, e') =
      if e' = zero then e else
      Sub(e,e')
    in
    let mul(e, e') =
      if e = zero then zero else
      if e' = zero then zero else
      if e = one then e' else
      if e' = one then e else
      Mul(e,e')
    in
    let div(e, e') =
      if e = zero then zero else
      if e' = one then e else
      Div(e,e')
    in
    let pow(e, n) =
      if e = zero then zero else
      if e = one || n = 0 then one else
      if n = 1 then e else
      Pow(e,n)
    in

    let rec fn = function
	Var name' -> if name = name' then one else zero
      | Cst _ -> zero
      | Add (e,e') -> add (fn e, fn e')
      | Sub (e,e') -> sub (fn e, fn e')
      | Mul (e,e') -> add (gn e e', gn e' e)
      | Div (e,e') -> sub (gn (Div(Cst R.one,e')) e, gn (Div(e, Pow (e',2))) e')
      | Pow (e,n) -> mul (Cst (R.t_of_int(n)), mul (fn e, pow (e,(n-1))))
      | Tra (trindex,e) ->
	  let tran = !trans_array.(trindex) in
	  mul ((subst tran.deriv [tran.name_var, e]), fn e)
      | Tra2 (trindex,e,e') ->
	  let tran = !trans2_array.(trindex) in
	  add (
	  mul ((subst tran.deriv1 [tran.name_var1, e; tran.name_var2, e']), fn e),
	  mul ((subst tran.deriv2 [tran.name_var1, e; tran.name_var2, e']), fn e'))
    and gn acc = function
    | Mul (e,e') ->
	add (gn (mul(acc, e')) e, gn (mul(acc, e)) e')
    | Div (e,e') ->
	sub(gn (div(acc, e')) e, gn (mul(acc, div(e, Pow (e',2)))) e')
    | e ->
	mul(acc, fn e)

    in
    simplify(fn expr)

  let rec sder fn var  = function
      0 -> fn
    | n -> sder (derive fn var) var (n-1)

  let laplacien fn vars =
    List.fold_left (fun acc var ->
		    let p = sder fn var 2 in
		    Add(p,acc)) zero vars

  let decompose_poly e =
    let e = try developS R.one (simplS (totypeS e)) with _ -> raise Exit in
    let rec fn e =
      match e with
	Pro _ | Var' _ | Cst' _ as e -> fn (Som [R.one, e])
      | Tra' _ | Tra2' _ -> raise Exit
      | Som l ->
	  List.map gn l
    and gn (c,e) =
      match e with
	Var' _ | Cst' _ as e -> gn (c, Pro [1, e])
      | Tra' _ | Tra2' _ | Som _ -> raise Exit
      | Pro l' ->
	  let c, m = List.fold_left hn (c, []) l' in
	  c, m
    and hn (c, acc) (i,e) =
      match e with
	Var' v -> c, (v, i)::acc
      | Cst' c' -> R.( ** ) c (eval_power c' i), acc
      | Tra' _ | Tra2' _ | Som _ | Pro _ -> raise Exit
    in
    fn e

  let make_poly e =
    let l = decompose_poly e in
    let l = List.filter (fun (_,l) -> List.for_all (fun (_,n) -> n >= 0) l) l in
    let e =
      List.fold_left (fun acc (c,l) ->
			let p = List.fold_left (fun acc (v,i) ->
						  Mul(acc,Pow(Var v,i))) (Cst R.one) l in
			Add(acc,Mul(Cst c, p))) (Cst R.zero) l in
    simplify e

  type prio = PAtm | PPow | PMul | PAdd

  let%parser ident =
    (x::RE"[a-zA-Z_][a-zA-Z0-9_]*") => x

  let not_digit _ b i _ _ =
    let (c,_,_) = Pacomb.Input.read b i in c <> '.' && (c < '0' || c > '9')

  let%parser rec expr p =
    PAtm < PPow < PMul < PAdd
    ; (p=PAtm) (x::R.parse) => Cst x
    ; (p=PAtm) (x::ident) =>
        (try
	  let f = StringMap.find x !fun_table in
	  if List.length f.variables <> 0 then
	    Pacomb.Lex.give_up
              ~msg:("wrong nummber of arguments for function "^x) ()
          else
            f.value
        with Not_found -> Var x)
    ; (p=PAtm) '(' (e::expr PAdd) ')'            => e
    ; (p=PPow) (e::expr PPow) '^' (n::INT)       => Pow(e, n)
    ; (p=PMul) (Pacomb.Grammar.test_after not_digit ('-' => ())) (f::expr PPow)  =>
                      Mul(Cst (R.opp(R.one)),f)
    ; (p=PMul) (e::expr PMul) '*' (f::expr PPow) => Mul(e,f)
    ; (p=PMul) (e::expr PMul) '/' (f::expr PPow) => Div(e,f)
    ; (p=PAdd) (e::expr PAdd) '+' (f::expr PMul) => Add(e,f)
    ; (p=PAdd) (e::expr PAdd) '-' (f::expr PMul) => Sub(e,f)
    ; (p=PAtm) (x::ident) '(' (args:: ~*[','] (expr PAdd)) ')' =>
	begin
          if List.mem x ["derive"; "simplify"; "filter_polynome";
                         "develop"]
          then Pacomb.Lex.give_up ();
	  try
	    let f = StringMap.find x !fun_table in
	    if List.length f.variables <> List.length args then
	      Pacomb.Lex.give_up
                ~msg:("wrong nummber of arguments for function "^x) ()
            else
	      let v = List.combine f.variables args in
	      subst f.value v
	  with Not_found -> try
	      match args with
		[e2] ->
		 Tra(StringMap.find x !trans_table, e2)
	      | [e2;e3] ->
		 Tra2(StringMap.find x !trans2_table, e2, e3)
	      |	_ ->
		 Pacomb.Lex.give_up
                   ~msg:("wrong nummber of arguments for function "^x) ()
	    with Not_found ->
	      Pacomb.Lex.give_up
	        ~msg:("unbound function: "^x) ()
	end
    ; (p=PAtm) "derive" '(' (e::expr PAdd) ',' (v::RE"[a-zA-Z_][a-zA-Z0-9_]*") ')'
        => simplify (derive e v)
    ; (p=PAtm) "simplify" '(' (e::expr PAdd) ')'
        => simplify e
    ; (p=PAtm) "develop" '(' (e::expr PAdd) ')'
        => develop e
    ; (p=PAtm) "filter_polynome" '(' (e::expr PAdd) ')'
      => make_poly e
    ; (p=PPow) (e::expr PPow) '['
        (l :: ~*[','] ((x::ident) "<-" (e::expr PAdd) => (x,e)))
        ']' => subst e l

  let parse = expr PAdd




end
