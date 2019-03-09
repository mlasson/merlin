module Reader : sig
  type t 
  val from_channel : in_channel -> t
  val read : timeout:float -> t -> string option
end

module Writer : sig
  type t
  val from_channel : out_channel -> t
  val write : t -> string -> unit
  val flush : timeout:float -> t -> bool
end