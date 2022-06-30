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

  val empty : t
  val read : In_channel.t -> t option
  val write : Out_channel.t -> t -> unit
end

module Contents: sig
  type t = unit -> string option
end

val read: In_channel.t -> (Header.t * Contents.t) option
val read_all: In_channel.t -> (Header.t * Contents.t) list
val write: Out_channel.t -> Header.t -> string -> unit
