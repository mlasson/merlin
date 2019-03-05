module Reader : sig
  type t 
  val from_channel : in_channel -> t
  val read : timeout:float -> t -> string option
end