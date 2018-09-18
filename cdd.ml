include Base

module type VECTOR = sig
  exception OutOfBounds
  type 'a t
  val dimension : int
  val get : 'a t -> int -> 'a option
  val get_exn : 'a t -> int -> 'a
  val vector : 'a list -> 'a t option
  val vector_exn : 'a list -> 'a t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val equiv : 'a t -> 'a t -> equal:('a -> 'a -> bool) -> bool
  val init : int -> f:(int -> 'a) -> 'a t
  val print :  'a t -> f:('a -> unit) -> unit
  val format : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type DIMENSION = sig
  val dimension : int
end
                       
module ArrayVector (D:DIMENSION) : VECTOR =
struct
  exception OutOfBounds
  type 'a t = 'a list
  let dimension = D.dimension
  let get_exn l n =
    if n < 0 || n > dimension - 1 then raise OutOfBounds
    else List.nth_exn l n
  let get l n = try Some (get_exn l n) with OutOfBounds -> None
  let vector_exn l =
    if List.length l <> dimension then raise OutOfBounds
    else l
  let vector l = try Some (vector_exn l) with OutOfBounds -> None
  let map2 = List.map2_exn
  let equiv = List.equal
  let init = List.init
  let print = List.iter
  let format (fmt_elt:(Format.formatter -> 'a -> unit)) fmt (lst:'a t) = 
    Format.fprintf fmt "[";
    print lst (fun i -> Format.fprintf fmt "%a; " fmt_elt i);
    Format.fprintf fmt "]"
end

(*
  val format : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a stack -> unit
end

module ListStack : Stack = struct
  type 'a stack = 'a list
  (* ... all the usual operations ... *)
  let format fmt_elt fmt s =
    Format.fprintf fmt "[";
    List.iter (fun elt -> Format.fprintf fmt "%a; " fmt_elt elt) s; 
    Format.fprintf fmt "]"
e
*)

module type CDD = functor (V:VECTOR) ->
sig
  type t
       [@@deriving sexp, show]
       
  type ternary = True | False | Unknown
                                  
  val leaf : ternary V.t -> t
                  
  val ite : int -> t -> t -> t
                                   
  val seq : t -> t -> t
                        
  val equiv : t -> t -> bool
                          
  val eval : t -> bool V.t -> ternary V.t
                                      
  val ( * ) : bool V.t -> ternary V.t -> bool V.t

  val format : Format.formatter -> t -> unit
end
  
module Make : CDD = functor (V:VECTOR) ->
struct
  type ternary = True | False | Unknown
  let cast (tern:ternary) : bool =
    match tern with
    | True -> true
    | _ -> false
  
  (* let compare tb1 tb2 = if  *)
  (* ternary bit addition *)
  let ( ++ ) tb1 tb2 = 
    match tb1, tb2 with
    | True, True | False, False | Unknown, Unknown -> tb1
    | (True, _) -> True
    | (_, True) -> True
    | (False, _) -> False
    | (_, False) -> False

  (* ternary bit multiplication *)
  let ( ** ) (bb1:bool) tb2 =
    match bb1, tb2 with
    | _, Unknown -> bb1
    | _,_ -> cast tb2
  type t =
    | Leaf of ternary V.t * ternary V.t
    | Branch of int * ternary V.t * t * t
  let leaf tv =
    let st_vec = List.init V.dimension (fun x -> Unknown) in
    Leaf (V.vector_exn st_vec, tv)

  (* equivalence of ternary bits *)
  let ( =& ) (a:ternary) (b:ternary) =
    match a,b with
    | True, True | False, False | Unknown, Unknown -> true
    | _ -> false

  
  let well_formed u v = 
    let f (b0:ternary) (b1:ternary) = if b0 =& b1 then Unknown else b1 in
    V.map2 u v f


  (* replace ternary vector tv with ternary bit tb in position k *)
  let replace (tv:ternary V.t) (k:int) (tb:ternary) : ternary V.t =
    let f i = if i = k then tb else (V.get_exn tv i) in
    V.init V.dimension f

  let rec correct_context idx updated_bit = function
    | Leaf (g, l) -> let g' = replace g idx updated_bit in
                     Leaf (g', well_formed g' l)
    | Branch(i, g, t0, t1) -> let g' = replace g idx updated_bit in
                              Branch(i, g', correct_context idx updated_bit t0, 
                                            correct_context idx updated_bit t1) 


  let merge_leaf_exn (u:ternary V.t) (v:ternary V.t) i =
    let rec f n =
      if n < 0 then true
      else
        let ui = V.get_exn u n in
        let vi = V.get_exn v n in
        if n = i && 
           ((not (ui =& vi) && not (not (ui =& Unknown) && not (vi =& Unknown))) 
            || vi =& ui) then true && f (n - 1)
        else (vi =& ui) && f (n-1)
    in
    if f (V.dimension - 1) then Some (V.map2 u v ( ++ ))
    else None
  
  let merge (idx:int) (g:ternary V.t) (fls:t) (tru:t) =
    try
      (* attempt to merge *)
      let rec merge' g' fls' tru'=
        match fls', tru' with
        | Leaf (g1, u), Leaf (g2, v) ->
        begin
          match (merge_leaf_exn u v idx) with
          | Some l -> Leaf (g', l)
          | None -> raise (Failure "merge")
        end
        | Leaf (g1, u), Branch (j, g2, t0, t1) -> 
            let context_0 = replace g' j False in
            let context_1 = replace g' j True in
            (* correct for leaf context as well *)
            Branch(j, g', (merge' context_0 (Leaf (g1, u)) t0),
                          (merge' context_1 (Leaf (g1, u)) t1))
        | Branch (j, g1, t0, t1), Leaf (g2, v) ->
            let context_0 = replace g' j False in
            let context_1 = replace g' j True in
            Branch(j, g', (merge' context_0 t0 (Leaf (g2, v))),
                          (merge' context_1 t1 (Leaf (g2, v))))
        | Branch (k, g1, t0, t1), Branch (j, g2, t0', t1') -> 
            if k = j then
              let context_0 = replace g' j False in
              let context_1 = replace g' j True in
              Branch(j, g', (merge' context_0 t0 t0'),
                            (merge' context_1 t1 t1'))
            else if k < j then
              let context_0 = replace g' k False in
              let context_1 = replace g' k True in
              Branch(k, g', (merge' context_0 t0 (Branch (j, g2, t0', t1'))), 
                            (merge' context_1 t1 (Branch (j, g2, t0', t1'))))
            else
              let context_0 = replace g' j False in
              let context_1 = replace g' j True in
              Branch(j, g', (merge' context_0 (Branch (k, g1, t0, t1)) t0'),
                            (merge' context_1 (Branch (k, g1, t0, t1)) t1'))
      in
      Some (merge' g fls tru)
    with Failure m -> None

  
  let ite (idx:int) (fls:t) (tru:t) =
    let g = (V.init V.dimension (fun i -> Unknown)) in
    match (merge idx g fls tru) with
    | Some t -> t
    | None -> Branch(idx, g, (correct_context idx False fls), 
                             (correct_context idx True tru))


  let seq t1 t2 = failwith "Unimplemented"


  let rec equiv t1 t2 = match t1, t2 with
    | Leaf (g1, u), Leaf (g2, v) -> V.equiv g1 g2 (=&) && V.equiv u v (=&)
    | Branch (i, g, t0, t1), Branch (j, g', t0', t1') -> 
      i = j && V.equiv g g' (=&) && equiv t0 t0' && equiv t1 t1'
    | _ -> false
  let rec eval t (bv:bool V.t) = 
    match t with
    | Leaf (_, v) -> v
    | Branch (i, _, t0, t1) -> 
      if (V.get_exn bv i) then eval t1 bv else eval t0 bv
  let ( * ) bv tv = V.map2 bv tv ( ** )

  let rec format fmt d =
    let print_elm = function
    | True -> Format.fprintf fmt "True;";
    | False -> Format.fprintf fmt "False;";
    | Unknown -> Format.fprintf fmt "Unknown;"; in
    match d with
    | Leaf(g, u) -> Format.fprintf fmt "Leaf (";
                    Format.fprintf fmt "[";
                    V.print g print_elm;
                    Format.fprintf fmt "], [";
                    V.print u print_elm;
                    Format.fprintf fmt "])"
    | Branch(i, g, t0, t1) -> Format.fprintf fmt "Branch (";
                              print_int i;
                              Format.fprintf fmt "[";
                              V.print g print_elm;
                              Format.fprintf fmt "],";
                              format fmt t0;
                              Format.fprintf fmt ",";
                              format fmt t1;
                              Format.fprintf fmt ")";

end