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

open Format (* ocaml module implements a pretty-printinting facilitity *)
open Pacomb

type lang = Human | Glsl | C

module type Set =
sig
  type elem

  val (==) : elem -> elem -> bool
      (* equality *)

  val normalize : elem -> elem
      (* there may be more than one
	 representation of each element in the set,
	 this function may perform some simplification
      *)

  val print : elem -> unit
      (* printing (using the Format library) *)
  val write : ?lang:lang -> formatter -> elem -> unit
      (* writing in file (using the Format library) *)
  val parse : elem Grammar.t
  val write_bin : out_channel -> elem -> unit
      (* writing as binary value *)
  val read_bin : in_channel -> elem
      (* reading as binary value *)
end

module type Group =
sig
  include Set

  val zero : elem
    (* zero ! *)
  val (++) : elem -> elem -> elem
      (* sum *)
  val (--) : elem -> elem -> elem
      (* subtraction *)
  val opp : elem -> elem
      (* opposite (0 -- x) *)
end

module type Ring =
sig
  include Group

  val one : elem
    (* one ! *)
  val t_of_int : int -> elem
      (* the standard mapping from integer *)
  val ( ** ) : elem -> elem -> elem
      (* product *)
  val conjugate : elem -> elem
      (* conjugate value *)
end


module type Euclidian_Ring =
sig
  include Ring
  type stathm
    (* type of the stathm of an element of the ring *)

  val stathm : elem -> stathm
      (* the stathm function *)
  val less : stathm -> stathm -> bool
      (* comparison of stathm (strict) *)

  val (//) : elem -> elem -> elem
      (* euclidian division *)
  val (mod) : elem -> elem -> elem
      (* remaining of the euclidian division *)
  val div_mod : elem -> elem -> elem * elem
      (* div_mod x y = (x // y, x mod y) *)
end

module type Field =
sig
  include Ring
  val inv : elem -> elem             (* inverse in the filed *)
  val (//) : elem -> elem -> elem       (* division in the field *)
end

module type Normed_Field =
sig
  include Field
  type norm
    (* type of the norm of an element of the ring *)

  val norm : elem -> norm
      (* the norm function (the square of the norm to be exact) *)
  val sqrt : norm -> norm
      (* the sqrt function if available *)
  val abs : elem -> norm
      (* should be the
	 square root of the norm if it exists. *)
  val leq : norm -> norm -> bool
      (* comparison of norm *)
  val add_norm : norm -> norm -> norm
  val zero_norm : norm
end

module type Vectorial_Space =
sig
  include Group

  type scalar
  val ( @ ) : scalar -> elem -> elem
  val conjugate : elem -> elem
      (* conjugate value *)
end

module type Banach =
sig
  include Vectorial_Space

  type norm
    (* type of the norm of an element of the ring *)

  val norm : elem -> norm
      (* the norm function (the square of the norm to be exact) *)
  val abs : elem -> norm
      (* sqrt of the norm if it exists *)
  val (|.) : elem -> elem -> scalar
      (* scalar product *)
end

module type Mapping =
sig
  type elem
  type argument
  type result

  val apply : elem -> argument -> result (* mult by a vector  *)
end

(****************************************************************************)
(*                    Module EUCLIDIAN Ring of Caml integer                 *)
(****************************************************************************)

(*
module Ring_Z =
  struct
    type elem = int
    type stathm = int
    let zero = 0
    let one = 1
    let t_of_int n = n
    let (++) = (+)
    let (--) = (-)
    let ( ** ) = ( * )
    let ( == ) = ( = )
    let opp x = - x
    let print = print_int
    let write ?(lang=Human) ch = pp_print_int ch
    let%parser parse = (n::INT)  => n
    let write_bin = output_value
    let read_bin = input_value
    let stathm = abs
    let less = (<)
    let (//) = (/)
    let (mod) = (mod)
    let div_mod x y = x / y, x mod y
    let normalize x = x
    let conjugate x = x
  end
 *)
(****************************************************************************)
(*             Module EUCLIDIAN Ring of multi-precision integer             *)
(****************************************************************************)

(*
module Ring_MZ =
  struct
    open Z

    type elem = t
    type stathm = t
    let zero = zero
    let one = one
    let t_of_int = of_int
    let (++) = add
    let (--) = sub
    let ( ** ) = mul
    let ( == ) = equal
    let opp = ( ~- )
    let print = print
    let write ?(lang=Human) = output
    let%parser parse = (str::RE"-?[0-9]+") => of_string str
    let write_bin = output_value
    let read_bin = input_value
    let stathm = abs
    let less = lt
    let (//) = div
    let (mod) = (mod)
    let div_mod = div_rem
    let normalize x = x
    let conjugate x = x
  end
 *)

(****************************************************************************)
(*                    Module Normed  Field of Caml float                    *)
(****************************************************************************)

module Field_R =
  struct
    type elem = float
    type norm = float
    let zero = 0.0
    let one = 1.0
    let t_of_int n = (float)n
    let (++) = (+.)
    let (--) = (-.)
    let ( ** ) = ( *. )
    let inv x = one /. x
    let (//) = (/.)
    let ( == ) = ( = )
    let opp x = -. x
    let print x = printf "%.19e" x
    let write ?(lang=Human) =
      let format:_ format4 = match lang with
        | Human -> "%f"
        | C -> "%.19e"
        | Glsl -> "%.10e"
      in
      (fun ch x -> fprintf ch format x)
    let%parser parse = (x::FLOAT) => x
    let write_bin = output_value
    let read_bin = input_value
    let norm x = x ** x
    let leq = (<=)
    let add_norm = (++)
    let zero_norm = zero
    let sqrt = sqrt
    let abs x = abs_float x
    let normalize x = x
    let conjugate x = x
  end


(****************************************************************************)
(*                    Module Normed  Field of GNU MPFR float                    *)
(****************************************************************************)

(*
module type Prec = sig val prec : int end

let _ =
  let open Mlmpfr in
  let x = make_from_int ~prec:200 13546115 in
  let s = Marshal.to_string x [] in
  let y = Marshal.from_string s 0 in
  assert (x = y)

module Field_MPFR = functor (P : Prec) ->
  struct
    open Mlmpfr
    type t = mpfr_float
    open P
    type elem = t
    type norm = t
    let zero = make_zero ~prec Positive
    let one = make_from_int ~prec 1
    let t_of_int n = make_from_int ~prec n
    let (++) x y = add ~prec x y
    let (--) x y = sub ~prec x y
    let ( ** ) x y = mul ~prec x y
    let (//) x y = div ~prec x y
    let inv x = one // x
    let ( == ) x y = cmp x y = 0
    let opp x = zero -- x
    let write ?(lang=Human) ch x = (* FIXME: use lang, but how ? *)
      let s, exp = get_str ~base:10 ~size:19 x in
      fprintf ch "%sE%s" s exp
    let print = write std_formatter
    let%parser parse = (str::RE"[+-]?[0-9]*[.][0-9]*([eE][+-]?[0-9]+)?")
      => Mlmpfr.make_from_str ~prec str
    let write_bin = output_value
    let read_bin = input_value
    let norm x = x ** x
    let leq x y = compare x y <= 0
    let add_norm = (++)
    let zero_norm = zero
    let sqrt x = sqrt ~prec x
    let abs x = abs ~prec x
    let normalize x = x
    let conjugate x = x
  end
 *)

(****************************************************************************)
(*                 Module Field of multi-precision rationnal                *)
(****************************************************************************)

(*
module Field_Q =
  struct
    open Q

    type elem = t
    type norm = t
    let zero = zero
    let one = one
    let t_of_int = of_int
    let (++) = (+)
    let (--) = (-)
    let ( ** ) = ( * )
    let inv x = one / x
    let (//) = (/)
    let ( == ) = equal
    let opp = ( ~- )
    let normalize x = x
    let print n = print_string (to_string n)
    let write ?(lang=Human) ch n = pp_print_string ch (to_string n)
    let%parser parse =
        (n::Ring_MZ.parse) => Q.of_bigint n
      ; (n::Ring_MZ.parse) '/' (d::Ring_MZ.parse)
          => Q.of_bigint n / Q.of_bigint d
    let write_bin = output_value
    let read_bin = input_value
    let norm = abs
    let leq = leq
    let add_norm = (++)
    let zero_norm = zero
    let sqrt _ = failwith "No sqrt in Field_Q"
    let abs = abs
    let conjugate x = x
  end
 *)

(****************************************************************************)
(*                    Module Field of complex numbers                       *)
(****************************************************************************)

module type Atype =
  sig
    type t
  end

module Field_CP = functor (T : Atype) ->
  functor (R : Normed_Field with type elem = T.t and type norm = T.t) ->
  struct

    type elem = R.elem * R.elem
    type norm = R.norm

    let zero = R.zero, R.zero
    let one = R.one, R.zero
    let t_of_int n = R.t_of_int n, R.zero
    let (++) (x,y) (x',y') = (R.(++) x x', R.(++) y y')
    let (--) (x,y) (x',y') = (R.(--) x x', R.(--) y y')
    let ( ** ) (x,y) (x',y') = (R.(--) (R.( ** ) x x') (R.( ** ) y y'),
                               R.(++) (R.( ** ) x y') (R.( ** ) y x'))
    let inv (x,y) =
      let d = R.(++) (R.( ** ) x x) (R.( ** ) y y) in
      (R.(//) x d, R.(//) (R.opp y) d)
    let (//) (x,y) (x',y') =
      let d = R.(++) (R.( ** ) x' x') (R.( ** ) y' y') in
      (R.(//) (R.(++) (R.( ** ) x x') (R.( ** ) y y')) d,
       R.(//) (R.(--) (R.( ** ) y x') (R.( ** ) x y')) d)
    let (==) (x,y) (x',y') = R.(==) x x' && R.(==) y y'
    let write ?(lang=Human) =
      let write = R.write ~lang in
      (fun formatter (x,y) ->
        pp_open_box formatter 2;  (* using  Format module *)
        write formatter x;
        pp_print_space formatter ();
        pp_print_string formatter "+";
        pp_print_space formatter ();
        pp_print_string formatter "i";
        write formatter y;
        pp_close_box formatter ())
    let print = write std_formatter
    let write_bin ch (x,y) =
      R.write_bin ch x; R.write_bin ch y
    let read_bin ch =
      let x = R.read_bin ch in
      let y = R.read_bin ch in
      x, y
    let%parser parse =
      (r :: R.parse) => (r, R.zero)
    ; (r :: R.parse) '+' 'i' (i :: (~?[R.zero] R.parse)) => (r, i)
    ; 'i' (i :: (~?[R.zero] R.parse)) => (R.zero, i)
    let opp (x,y) = (R.opp x, R.opp y)
    let norm (x,y) = R.(++) (R.( ** ) x x) (R.( ** ) y y)
    let conjugate (x,y) = (x, R.opp y)
    let sqrt = R.sqrt
    let abs z = R.sqrt (norm z)
    let leq = R.leq
    let add_norm = R.add_norm
    let zero_norm = R.zero_norm
    let normalize x = x
  end

module Field_C  = Field_CP (struct type t = float end)(Field_R)
(*module Field_QC = Field_CP (struct type t = Q.t end)(Field_Q)*)

(* missing : The euclidian ring of Gauss integer *)

module Pgcd = functor (R : Euclidian_Ring) ->
  struct
    open R
      let rec pgcd a b =
      let r = a mod b in
      if r == zero then b else pgcd b r
      let rec bezout a b =
      let q, r = div_mod a b in
      if r == zero then b, R.zero, R.one else
      let p, u, v = bezout b r in
      p, v, u -- (v ** q)
  end

module type One_element =
  sig
    type elem
    val elt : elem
  end

module Quotient =
  functor (R : Euclidian_Ring) ->
  functor (Elt : One_element with type elem = R.elem) ->
  struct
		include R
    let (==) a b = R.(==) (R.(mod) (R.(--) a b) Elt.elt) R.zero
    let normalize x = R.(mod) (R.normalize x) Elt.elt
    let print x = R.print (normalize x)
    let write ?(lang=Human) =
      let write = R.write ~lang in
      fun ch x -> write ch (normalize x)
    let write_bin ch x = R.write_bin ch (normalize x)
  end

module Quotient_prime =
  functor (R : Euclidian_Ring) ->
  functor (Elt : One_element with type elem = R.elem) ->
  struct
    module P = Pgcd(R)
    module Q = Quotient(R)(Elt)

    include Q
    let inv x =
      let _,u,_ = P.bezout x Elt.elt in
      u
    let (//) x y = x ** inv y
  end
