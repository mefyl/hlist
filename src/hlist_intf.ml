module type S_BASE = sig
  type 'a value

  type 'a t =
    | ( :: ) : 'hd value * 'tl t -> ('hd -> 'tl) t
    | [] : unit t

  type equal = { f : 'a. 'a value -> 'a value -> bool }
  type 'res mapper = { f : 'a. 'a value -> 'res }
  type 'res imapper = { f : 'a. int -> 'a value -> 'res }
  type printer = { f : 'a. Format.formatter -> 'a value -> unit }
end

module type S2_BASE = sig
  type ('a, 'b) value

  type ('a, 'b) t =
    | ( :: ) : ('hd, 'b) value * ('tl, 'b) t -> ('hd -> 'tl, 'b) t
    | [] : (unit, 'b) t

  type equal = { f : 'a 'b. ('a, 'b) value -> ('a, 'b) value -> bool }
  type ('b, 'res) mapper = { f : 'a. ('a, 'b) value -> 'res }
  type ('b, 'res) imapper = { f : 'a. int -> ('a, 'b) value -> 'res }
  type printer = { f : 'a 'b. Format.formatter -> ('a, 'b) value -> unit }
end

module type S2_INTF = sig
  type ('a, 'b) t
  type ('a, 'b) value
  type ('b, 'res) mapper
  type ('b, 'res) imapper
  type equal
  type printer

  val equal : equal -> ('a, 'b) t -> ('a, 'b) t -> bool

  val pp :
    printer ->
    ?sep:(Format.formatter -> unit -> unit) ->
    Format.formatter ->
    _ t ->
    unit

  val length : _ t -> int
  val filter_map : ('a, 'b) t -> f:('b, 'res option) mapper -> 'res list
  val filter_mapi : ('a, 'b) t -> f:('b, 'res option) imapper -> 'res list
  val find_map : ('a, 'b) t -> f:('b, 'res option) mapper -> 'res option
  val find_mapi : ('a, 'b) t -> f:('b, 'res option) imapper -> 'res option
  val iter : ('a, 'b) t -> f:('b, unit) mapper -> unit
  val iteri : ('a, 'b) t -> f:('b, unit) imapper -> unit
  val map : ('a, 'b) t -> f:('b, 'res) mapper -> 'res list
  val mapi : ('a, 'b) t -> f:('b, 'res) imapper -> 'res list

  module Transduce (To : S_BASE) : sig
    type mapper = { f : 'a 'b. ('a, 'b) value -> 'a To.value }

    val map : ('a, 'b) t -> f:mapper -> 'a To.t

    type imapper = { f : 'a 'b. int -> ('a, 'b) value -> 'a To.value }

    val mapi : ('a, 'b) t -> f:imapper -> 'a To.t
  end

  module Transduce2 (To : S2_BASE) : sig
    type mapper = { f : 'a 'b1 'b2. ('a, 'b1) value -> ('a, 'b2) To.value }

    val map : ('a, 'b1) t -> f:mapper -> ('a, 'b2) To.t

    type imapper = {
      f : 'a 'b1 'b2. int -> ('a, 'b1) value -> ('a, 'b2) To.value;
    }

    val mapi : ('a, 'b1) t -> f:imapper -> ('a, 'b2) To.t
  end
end

module type S2 = sig
  include S2_BASE

  include
    S2_INTF
      with type ('a, 'b) t := ('a, 'b) t
       and type ('a, 'b) value := ('a, 'b) value
       and type ('a, 'b) mapper := ('a, 'b) mapper
       and type ('a, 'b) imapper := ('a, 'b) imapper
       and type equal := equal
       and type printer := printer
end

module type S = sig
  include S_BASE

  include
    S2_INTF
      with type ('a, _) t := 'a t
       and type ('a, _) value := 'a value
       and type (_, 'a) mapper := 'a mapper
       and type (_, 'a) imapper := 'a imapper
       and type equal := equal
       and type printer := printer
end
