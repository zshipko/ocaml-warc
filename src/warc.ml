module Header = struct
  module Warctype = struct
    type t =
      | Warcinfo
      | Response
      | Resource
      | Request
      | Metadata
      | Revisit
      | Conversion
      | Continuation

    let of_string s =
      match String.lowercase_ascii s with
      | "warcinfo" -> Warcinfo
      | "response" -> Response
      | "resource" -> Resource
      | "request" -> Request
      | "metadata" -> Metadata
      | "revisit" -> Revisit
      | "conversion" -> Conversion
      | "continuation" -> Continuation
      | _ -> invalid_arg "Warctype.of_string"

    let to_string = function
      | Warcinfo -> "warcinfo"
      | Response -> "response"
      | Resource -> "resource"
      | Request -> "request"
      | Metadata -> "metadata"
      | Revisit -> "revisit"
      | Conversion -> "conversion"
      | Continuation -> "continuation"
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

  let split_key_value line =
    try
      let idx = String.index line ':' in
      let idx' = idx + 1 in
      let len = String.length line in
      let key = String.trim @@ String.sub line 0 idx in
      let value = String.trim @@ String.sub line idx' (len - idx') in
      Some (key, value)
    with Not_found -> None

  let read_line ic =
    match In_channel.input_line ic with
    | None -> None
    | Some line ->
        let line = String.trim line in
        Some line

  let read_field ic =
    let line = read_line ic in
    match line with
    | None -> None
    | Some line ->
        let is_empty = String.length line = 0 in
        if is_empty then None else split_key_value line

  let empty =
    {
      version = "";
      id = "";
      content_length = 0L;
      content_type = "application/octet-stream";
      date = "";
      warctype = Warcinfo;
      target_uri = None;
      extras = [];
    }

  let rec read ic =
    let rec inner t =
      match read_field ic with
      | None -> t
      | Some ("WARC-Type", ty) ->
          inner { t with warctype = Warctype.of_string ty }
      | Some ("WARC-Record-ID", id) -> inner { t with id }
      | Some ("Content-Length", n) ->
          inner { t with content_length = Int64.of_string n }
      | Some ("Content-Type", content_type) -> inner { t with content_type }
      | Some ("WARC-Date", date) -> inner { t with date }
      | Some ("WARC-Target-URI", uri) -> inner { t with target_uri = Some uri }
      | Some (k, v) -> inner { t with extras = (k, v) :: t.extras }
    in
    match read_line ic with
    | None -> None
    | Some "" -> read ic
    | Some version -> Some (inner { empty with version })

  let write_key_value oc k v =
    Out_channel.output_string oc k;
    Out_channel.output_string oc ": ";
    Out_channel.output_string oc v;
    Out_channel.output_char oc '\n'

  let write oc header =
    Out_channel.output_string oc header.version;
    Out_channel.output_char oc '\n';
    write_key_value oc "WARC-Type" (Warctype.to_string header.warctype);
    write_key_value oc "WARC-Record-ID" header.id;
    write_key_value oc "Content-Length" (Int64.to_string header.content_length);
    write_key_value oc "Content-Type" header.content_type;
    write_key_value oc "WARC-Date" header.date;
    Option.iter (write_key_value oc "WARC-Target-URI") header.target_uri;
    List.iter (fun (k, v) -> write_key_value oc k v) header.extras;
    Out_channel.output_string oc "\r\n"
end

module Contents = struct
  type t = unit -> string option

  let read ic header =
    In_channel.really_input_string ic
      (Int64.to_int header.Header.content_length - 2)

  let v ic header =
    let offset = In_channel.pos ic in
    In_channel.seek ic (Int64.add offset header.Header.content_length);
    fun () ->
      let curr = In_channel.pos ic in
      In_channel.seek ic offset;
      let r = read ic header in
      In_channel.seek ic curr;
      r
end

let read ic =
  let first = Int64.equal (In_channel.pos ic) 0L in
  let () =
    if not first then (
      ignore @@ Header.read_line ic;
      ignore @@ Header.read_line ic)
  in
  match Header.read ic with
  | None -> None
  | Some hdr -> Some (hdr, Contents.v ic hdr)

let read_all ic =
  let rec inner acc =
    match read ic with None -> acc | Some (hdr, c) -> inner ((hdr, c) :: acc)
  in
  List.rev (inner [])

let write oc header contents =
  Header.write oc header;
  Out_channel.output_string oc contents;
  Out_channel.output_string oc "\r\n";
  Out_channel.output_string oc "\r\n";
  Out_channel.output_string oc "\r\n"
