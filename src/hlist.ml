include Hlist_intf

module Make (T : sig
  type 'a t
end) : S with type 'a value = 'a T.t = struct
  module Base = struct
    type 't value = 't T.t

    type 't t =
      | ( :: ) : 'hd value * 'tl t -> ('hd, 'tl) desc t
      | [] : unit t
  end

  include Base

  let length l =
    let rec loop : type l. int -> l t -> int =
     fun acc -> function _ :: tl -> loop (acc + 1) tl | [] -> acc
    in
    loop 0 l

  type 'res imapper = { f : 't. int -> 't value -> 'res }

  let filter_mapi l ~(f : 'res option imapper) =
    let rec map : type a. int -> a t -> 'res list =
     fun i -> function
      | hd :: tl -> (
        match f.f i hd with
        | Some value -> List.cons value (map (i + 1) tl)
        | None -> map (i + 1) tl)
      | [] -> []
    in
    map 0 l

  let mapi l ~f = filter_mapi ~f:{ f = (fun i x -> Some (f.f i x)) } l

  let iteri l ~f =
    let (_ : unit list) = mapi ~f l in
    ()

  let find_mapi l ~(f : 'res option imapper) =
    let rec loop : type a. int -> a t -> 'res option =
     fun i -> function
      | hd :: tl -> (
        match f.f i hd with Some value -> Some value | None -> loop (i + 1) tl)
      | [] -> None
    in
    loop 0 l

  type 'res mapper = { f : 't. 't value -> 'res }

  let filter_map l ~(f : 'res option mapper) =
    filter_mapi l ~f:{ f = (fun _ v -> f.f v) }

  let find_map l ~(f : 'res option mapper) =
    find_mapi l ~f:{ f = (fun _ v -> f.f v) }

  let map l ~f = filter_map ~f:{ f = (fun x -> Some (f.f x)) } l
  let iter l ~f = map ~f l |> ignore

  module Transduce (To : S_BASE) = struct
    type imapper = { f : 't. int -> 't value -> 't To.value }

    let mapi l ~f:{ f } =
      let rec map : type a. int -> a t -> a To.t =
       fun i -> function hd :: tl -> f i hd :: map (i + 1) tl | [] -> []
      in
      map 0 l

    type mapper = { f : 't. 't value -> 't To.value }

    let map l ~f:{ f } = mapi l ~f:{ f = (fun _ v -> f v) }
  end

  module Transduce2 (To : S2_BASE) = struct
    type 't folding_mapper = {
      f : 'a 'b. 't -> 'a value -> 't * ('a, 'b) To.value;
    }

    let folding_map folded l ~f:{ f } =
      let rec map : type a b. 'fold -> a t -> (a, b) To.t =
       fun folded -> function
        | hd :: tl ->
          let folded, hd = f folded hd in
          hd :: map folded tl
        | [] -> []
      in
      map folded l

    type imapper = { f : 'a 'b. int -> 'a value -> ('a, 'b) To.value }

    let mapi l ~f:{ f } =
      folding_map 0 l ~f:{ f = (fun i elt -> (i + 1, f i elt)) }

    type mapper = { f : 'a 'b. 'a value -> ('a, 'b) To.value }

    let map l ~f:{ f } = mapi l ~f:{ f = (fun _ v -> f v) }
  end
end

include Make (struct
  type 'a t = 'a
end)

type (_, _) convert =
  | Cons : ('b, 'c) convert -> ('a -> 'b, 'a * 'c) convert
  | Null : (unit, unit) convert

let rec to_pairs : type a b. (a, b) convert -> a t -> b =
 fun convert l ->
  match (convert, l) with
  | Cons next, a :: b -> (a, to_pairs next b)
  | Null, [] -> ()

module Make2 (T : sig
  type ('a, 'b) t
end) : S2 with type ('a, 'b) value = ('a, 'b) T.t = struct
  type ('t, 'tag) value = ('t, 'tag) T.t

  type ('t, 'tag) t =
    | ( :: ) : ('hd, 'tag) value * ('tl, 'tag) t -> (('hd, 'tl) desc, 'tag) t
    | [] : (unit, 'tag) t

  let length l =
    let rec loop : type l. int -> (l, 'tag) t -> int =
     fun acc -> function _ :: tl -> loop (acc + 1) tl | [] -> acc
    in
    loop 0 l

  type ('b, 'res) imapper = { f : 'a. int -> ('a, 'b) value -> 'res }

  let filter_mapi l ~f =
    let rec map : type a. int -> (a, 'tag) t -> 'res list =
     fun i -> function
      | hd :: tl -> (
        match f.f i hd with
        | Some value -> List.cons value (map (i + 1) tl)
        | None -> map (i + 1) tl)
      | [] -> []
    in
    map 0 l

  let mapi l ~f = filter_mapi ~f:{ f = (fun i x -> Some (f.f i x)) } l

  let iteri l ~f =
    let (_ : unit list) = mapi ~f l in
    ()

  let find_mapi l ~(f : ('b, 'res option) imapper) =
    let rec loop : type a. int -> (a, 'b) t -> 'res option =
     fun i -> function
      | hd :: tl -> (
        match f.f i hd with Some value -> Some value | None -> loop (i + 1) tl)
      | [] -> None
    in
    loop 0 l

  type ('b, 'res) mapper = { f : 'a. ('a, 'b) value -> 'res }

  let filter_map l ~f = filter_mapi l ~f:{ f = (fun _ v -> f.f v) }

  let find_map l ~(f : ('b, 'res option) mapper) =
    find_mapi l ~f:{ f = (fun _ v -> f.f v) }

  let map l ~f = filter_map ~f:{ f = (fun x -> Some (f.f x)) } l
  let iter l ~f = map ~f l |> ignore

  module Transduce (To : S_BASE) = struct
    type imapper = { f : 'a 'b. int -> ('a, 'b) value -> 'a To.value }

    let mapi l ~f:{ f } =
      let rec map : type a b. int -> (a, b) t -> a To.t =
       fun i -> function hd :: tl -> f i hd :: map (i + 1) tl | [] -> []
      in
      map 0 l

    type mapper = { f : 'a 'b. ('a, 'b) value -> 'a To.value }

    let map l ~f:{ f } = mapi l ~f:{ f = (fun _ v -> f v) }
  end

  module Transduce2 (To : S2_BASE) = struct
    type imapper = { f : 'a 'b 'c. int -> ('a, 'b) value -> ('a, 'c) To.value }

    let mapi l ~f:{ f } =
      let rec map : type a b c. int -> (a, b) t -> (a, c) To.t =
       fun i -> function hd :: tl -> f i hd :: map (i + 1) tl | [] -> []
      in
      map 0 l

    type mapper = { f : 'a 'b 'c. ('a, 'b) value -> ('a, 'c) To.value }

    let map l ~f:{ f } = mapi l ~f:{ f = (fun _ v -> f v) }
  end
end

module Zip2 (L : S2) (R : S2) = struct
  include Make2 (struct
    type ('a, 'b) t = ('a, 'b) L.value * ('a, 'b) R.value
  end)

  let zip l r =
    let rec zip : type a. (a, 'b) L.t * (a, 'b) R.t -> (a, 'b) t = function
      | L.(hd_l :: tl_l), R.(hd_r :: tl_r) -> (hd_l, hd_r) :: zip (tl_l, tl_r)
      | L.[], R.[] -> []
    in
    zip (l, r)
end
