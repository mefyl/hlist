let deconstruct () =
  let l = Hlist.[ 0; "1"; Some "2"; Some 3; 4; 5; 6; 7 ] in
  let f Hlist.[ a; b; c; d; e; f; g; h ] =
    Fmt.(
      str "%i %s %a %a %i %i %i %i" a b (option string) c (option int) d e f g h)
  in
  Alcotest.(check ~here:[%here] int "length" 8 @@ Hlist.length l);
  Alcotest.(check ~here:[%here] string "deconstructed" "0 1 2 3 4 5 6 7" @@ f l)

type _ atom =
  | Int : int -> int atom
  | String : string -> string atom

let pp_atom : type a. Format.formatter -> a atom -> unit =
 fun fmt -> function
  | Int i -> Format.pp_print_int fmt i
  | String s -> Format.pp_print_string fmt s

let equal_atom : type a. a atom -> a atom -> bool =
 fun l r ->
  match (l, r) with
  | Int l, Int r -> l = r
  | String l, String r -> String.equal l r

module Atom_hlist = Hlist.Make (struct
  type 'a t = 'a atom
end)

let find_map () =
  let l =
    Atom_hlist.[ Int 0; String "nope"; String "1"; String "2"; Int 3; Int 4 ]
  in
  let f n =
    let f : type t. t atom -> int option = function
      | Int i when i = n -> Some i
      | Int _ -> None
      | String s -> (
        try
          let i = int_of_string s in
          if i = n then Some i else None
        with _ -> None)
    in
    ({ f } : int option Atom_hlist.mapper)
  in
  Alcotest.(
    check ~here:[%here] (option int) "found int" (Some 3)
      Atom_hlist.(find_map ~f:(f 3) l));
  Alcotest.(
    check ~here:[%here] (option int) "found string" (Some 2)
      Atom_hlist.(find_map ~f:(f 2) l));
  Alcotest.(
    check ~here:[%here] (option int) "not found" None
      Atom_hlist.(find_map ~f:(f 5) l))

module Type = struct
  type _ t =
    | Int : int t
    | String : string t

  let pp : type a. Format.formatter -> a t -> unit =
   fun fmt -> function
    | Int -> Format.pp_print_string fmt "int"
    | String -> Format.pp_print_string fmt "string"

  let equal : type a. a t -> a t -> bool =
   fun l r -> match (l, r) with Int, Int -> true | String, String -> true
end

module Type_hlist = Hlist.Make (struct
  type 'a t = 'a Type.t
end)

let to_types =
  let module To_types = Atom_hlist.Transduce (Type_hlist) in
  To_types.map
    ~f:
      {
        f =
          (fun (type t) (v : t atom) : t Type.t ->
            match v with Int _ -> Type.Int | String _ -> Type.String);
      }

let transduce () =
  let [ Int; String; Int ] = to_types [ Int 0; String "one"; Int 2 ] in
  ()

let equal_with_type (l, lt) (r, rt) = equal_atom l r && Type.equal lt rt

let pp_with_type fmt (a, t) =
  pp_atom fmt a;
  Format.pp_print_space fmt ();
  Type.pp fmt t

let zip () =
  let module Zip = Hlist.Zip (Atom_hlist) (Type_hlist) in
  let with_types l = Zip.zip l @@ to_types l in
  Alcotest.(
    check ~here:[%here]
      (testable
         (Zip.pp { f = pp_with_type })
         (Zip.equal { f = equal_with_type }))
      "zipped")
    [ (Int 0, Int); (String "one", String); (Int 2, Int) ]
  @@ with_types [ Int 0; String "one"; Int 2 ]

let () =
  let open Alcotest in
  run "Hlist"
    [
      ( "Hlist",
        [
          test_case "deconstruct" `Quick deconstruct;
          test_case "find_map" `Quick find_map;
          test_case "transduce" `Quick transduce;
          test_case "zip" `Quick zip;
        ] );
    ]
