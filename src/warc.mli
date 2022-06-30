module Header : sig
  module Warctype : sig
    type t =
      | Warcinfo
      | Response
      | Resource
      | Request
      | Metadata
      | Revisit
      | Conversion
      | Continuation

    val to_string : t -> string
    val of_string : string -> t
    val equal : t -> t -> bool
  end

  type t = {
    version : string;
    id : string;
    content_length : int64;
    content_type : string;
    date : string;
    warctype : Warctype.t;
    target_uri : string option;
    extras : (string * string) list;
  }
  
  val equal: t -> t -> bool

  val empty : t

  val v :
    ?version:string ->
    ?id:string ->
    ?content_length:int64 ->
    ?content_type:string ->
    ?date:string ->
    ?target_uri:string ->
    ?extras:(string * string) list ->
    Warctype.t ->
    t

  val read : In_channel.t -> t option
  val write : Out_channel.t -> t -> unit
  val pp: Format.formatter -> t -> unit
end

module Contents : sig
  type t = unit -> string option
end

val read : In_channel.t -> (Header.t * Contents.t) option
val read_all : In_channel.t -> (Header.t * Contents.t) list
val write : Out_channel.t -> Header.t -> string option -> unit
val write_all : Out_channel.t -> (Header.t * string option) list -> unit