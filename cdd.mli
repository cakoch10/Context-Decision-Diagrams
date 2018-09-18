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

module ArrayVector (D:DIMENSION) : VECTOR                           

module type CDD = functor (V:VECTOR) ->
sig
  type t
       [@@deriving sexp,show]
       
  type ternary = True | False | Unknown
                                  
  val leaf : ternary V.t -> t
                              
  val ite : int -> t -> t -> t
                                   
  val seq : t -> t -> t
                        
  val equiv : t -> t -> bool
                          
  val eval : t -> bool V.t -> ternary V.t
                                      
  val ( * ) : bool V.t -> ternary V.t -> bool V.t
  
  val format : Format.formatter -> t -> unit
end
  
module Make : CDD
