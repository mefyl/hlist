(** Heterogeneous lists. *)

include module type of Hlist_intf

module Make (T : sig
  type 'a t
end) : S with type 'a value = 'a T.t

module Make2 (T : sig
  type ('a, 'b) t
end) : S2 with type ('a, 'b) value = ('a, 'b) T.t

include module type of Select_schema.Hlist
include S_OPS with type 'a t := 'a Select_schema.Hlist.t and type 'a value := 'a

type (_, _) convert =
  | Cons : ('b, 'c) convert -> ('a -> 'b, 'a * 'c) convert
  | Null : (unit, unit) convert

val to_pairs : ('l, 'pairs) convert -> 'l t -> 'pairs

module Zip2 (L : S2) (R : S2) : sig
  include S2 with type ('a, 'b) value = ('a, 'b) L.value * ('a, 'b) R.value

  val zip : ('a, 'b) L.t -> ('a, 'b) R.t -> ('a, 'b) t
end

(* module Transduce (To : S_BASE) : sig
 *   type mapper = { f : 'a. 'a -> 'a To.value }
 *
 *   val map : 'a t -> f:mapper -> 'a To.t
 *
 *   type imapper = { f : 'a. int -> 'a -> 'a To.value }
 *
 *   val mapi : 'a t -> f:imapper -> 'a To.t
 * end
 *
 * module Transduce2 (To : S2_BASE) : sig
 *   type mapper = { f : 'a 'b. 'a -> ('a, 'b) To.value }
 *
 *   val map : 'a t -> f:mapper -> ('a, 'b) To.t
 *
 *   type imapper = { f : 'a 'b. int -> 'a value -> ('a, 'b) To.value }
 *
 *   val mapi : 'a t -> f:imapper -> ('a, 'b) To.t
 *
 *   type 't folding_mapper = {
 *     f : 'a 'b. 't -> 'a value -> 't * ('a, 'b) To.value;
 *   }
 *
 *   val folding_map : 'fold -> 'a t -> f:'fold folding_mapper -> ('a, 'b) To.t
 * end *)
