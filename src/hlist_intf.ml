type ('a, 'b) desc = 'a -> 'b

module type S_BASE = sig
  type 'a value

  type 'a t =
    | ( :: ) : 'hd value * 'tl t -> ('hd, 'tl) desc t
    | [] : unit t
end

module type S2_BASE = sig
  type ('a, 'b) value

  type ('a, 'b) t =
    | ( :: ) : ('hd, 'b) value * ('tl, 'b) t -> (('hd, 'tl) desc, 'b) t
    | [] : (unit, 'b) t
end

module type S = sig
  include S_BASE

  val length : _ t -> int

  type 'res mapper = { f : 'a. 'a value -> 'res }
  type 'res imapper = { f : 'a. int -> 'a value -> 'res }

  val filter_map : 'a t -> f:'res option mapper -> 'res list
  val filter_mapi : 'a t -> f:'res option imapper -> 'res list
  val find_map : 'a t -> f:'res option mapper -> 'res option
  val find_mapi : 'a t -> f:'res option imapper -> 'res option
  val iter : 'a t -> f:unit mapper -> unit
  val iteri : 'a t -> f:unit imapper -> unit
  val map : 'a t -> f:'res mapper -> 'res list
  val mapi : 'a t -> f:'res imapper -> 'res list

  module Transduce (To : S_BASE) : sig
    type mapper = { f : 'a. 'a value -> 'a To.value }

    val map : 'a t -> f:mapper -> 'a To.t

    type imapper = { f : 'a. int -> 'a value -> 'a To.value }

    val mapi : 'a t -> f:imapper -> 'a To.t
  end

  module Transduce2 (To : S2_BASE) : sig
    type mapper = { f : 'a 'b. 'a value -> ('a, 'b) To.value }

    val map : 'a t -> f:mapper -> ('a, 'b) To.t

    type imapper = { f : 'a 'b. int -> 'a value -> ('a, 'b) To.value }

    val mapi : 'a t -> f:imapper -> ('a, 'b) To.t

    type 't folding_mapper = {
      f : 'a 'b. 't -> 'a value -> 't * ('a, 'b) To.value;
    }

    val folding_map : 'fold -> 'a t -> f:'fold folding_mapper -> ('a, 'b) To.t
  end
end

module type S2 = sig
  include S2_BASE

  val length : _ t -> int

  type ('b, 'res) mapper = { f : 'a. ('a, 'b) value -> 'res }
  type ('b, 'res) imapper = { f : 'a. int -> ('a, 'b) value -> 'res }

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
