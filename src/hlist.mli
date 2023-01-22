(** Heterogeneous lists. *)

include module type of Hlist_intf

module Make (T : sig
  type 'a t
end) : S with type 'a value = 'a T.t

module Make2 (T : sig
  type ('a, 'b) t
end) : S2 with type ('a, 'b) value = ('a, 'b) T.t

include module type of Make (struct
  type 'a t = 'a
end)

type (_, _) convert =
  | Cons : ('b, 'c) convert -> ('a -> 'b, 'a * 'c) convert
  | Null : (unit, unit) convert

val to_pairs : ('l, 'pairs) convert -> 'l t -> 'pairs

module Zip2 (L : S2) (R : S2) : sig
  include S2 with type ('a, 'b) value = ('a, 'b) L.value * ('a, 'b) R.value

  val zip : ('a, 'b) L.t -> ('a, 'b) R.t -> ('a, 'b) t
end
