(** [warc] is library used to read and write web archive files. A streaming
    API is provided to avoid loading large contents into memory *)

module Header : sig
  (** The [Header] module allows you to encode and decode WARC headers *)

  module Warctype : sig
    (** The [Warctype] is used to encode/decode the [WARC-Type] field *)

    type t =
      | Warcinfo
      | Response
      | Resource
      | Request
      | Metadata
      | Revisit
      | Conversion
      | Continuation
          (** [Warctype.t] enumerates all possible values for the [WARC-Type] field *)

    val to_string : t -> string
    (** [to_string ty] is the string encoding of [t] *)

    val of_string : string -> t
    (** [of_string s] parses a string and returns the proper [Warctype.t], if
        the string is invalid then [Invalid_argument] will be raised *)

    val equal : t -> t -> bool
    (** [equal a b] is true when [a] and [b] are the same *)
  end

  type t = {
    version : string;  (** Version header, this comes before all other fields *)
    id : string;  (** [WARC-Record-ID], a unique identifier for each record *)
    content_length : int64;  (** Length contents in bytes *)
    content_type : string;  (** Content MIME type *)
    date : string;  (** Date and time in ISO8601 *)
    warctype : Warctype.t;  (** Specifies the type of record *)
    target_uri : string option;
        (** The URI used to make the original request *)
    extras : (string * string) list;  (** Optional additional fields *)
  }
  (** Header type *)

  val equal : t -> t -> bool
  (** [equal a b] is true when all fields of [a] and [b] are equal *)

  val empty : t
  (** Empty header *)

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
  (* Creates a new header with the default value for optional arguments that are not provided *)

  val read : In_channel.t -> t option
  (** [read ic] reads the next header from [ic] *)

  val write : Out_channel.t -> t -> unit
  (** [write oc header] encodes [header] and writes it to [oc] *)

  val pp : Format.formatter -> t -> unit
  (** [pp fmt header] encodes [header] and writes it to [fmt] *)
end

module Contents : sig
  type t = unit -> string option
  (** Lazy contents *)
end

val read : In_channel.t -> (Header.t * Contents.t) option
(** [read ic] returns the next {!Header.t} and {!Contents.t} from [ic] *)

val read_all : In_channel.t -> (Header.t * Contents.t) list
(** [read_all ic] returns a list of {!Header.t}/{!Contents.t} pairs for each entry in a WARC file *)

val write : Out_channel.t -> Header.t -> string option -> unit
(** [write oc header contents] writes [header] and [contents] to [oc] *)

val write_all : Out_channel.t -> (Header.t * string option) list -> unit
(** [write_all oc l] writes all entries from [l] to [oc] *)
