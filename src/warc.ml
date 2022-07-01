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

    let equal a b =
      match (a, b) with
      | Warcinfo, Warcinfo
      | Response, Response
      | Resource, Resource
      | Request, Request
      | Metadata, Metadata
      | Revisit, Revisit
      | Conversion, Conversion
      | Continuation, Continuation ->
          true
      | _ -> false
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

  let equal a b =
    String.equal a.version b.version
    && String.equal a.id b.id
    && Int64.equal a.content_length b.content_length
    && String.equal a.content_type b.content_type
    && String.equal a.date b.date
    && Warctype.equal a.warctype b.warctype
    && Option.equal String.equal a.target_uri b.target_uri
    && List.equal
         (fun (a, b) (x, y) -> String.equal a x && String.equal b y)
         a.extras b.extras

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

  let now () =
    let now = Unix.gettimeofday () in
    ISO8601.Permissive.string_of_datetime now ^ "Z"

  let uuid () =
    let id = Uuidm.v `V4 |> Uuidm.to_string in
    Printf.sprintf "<urn:uuid:%s>" id

  let v ?(version = "WARC/1.0") ?id ?(content_length = 0L)
      ?(content_type = empty.content_type) ?date ?target_uri ?(extras = [])
      warctype =
    let id = match id with None -> uuid () | Some id -> id in
    let date = match date with None -> now () | Some d -> d in
    {
      version;
      id;
      content_length;
      content_type;
      date;
      warctype;
      target_uri;
      extras;
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

  let pp_kv fmt k v = Format.fprintf fmt "%s: %s\r\n" k v

  let pp fmt header =
    Format.pp_print_string fmt header.version;
    Format.pp_print_string fmt "\r\n";
    pp_kv fmt "WARC-Type" (Warctype.to_string header.warctype);
    pp_kv fmt "WARC-Record-ID" header.id;
    pp_kv fmt "Content-Length" (Int64.to_string header.content_length);
    pp_kv fmt "Content-Type" header.content_type;
    pp_kv fmt "WARC-Date" header.date;
    Option.iter (pp_kv fmt "WARC-Target-URI") header.target_uri;
    List.iter (fun (k, v) -> pp_kv fmt k v) header.extras;
    Format.pp_print_string fmt "\r\n";
    Format.pp_print_flush fmt ()

  let write oc header =
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%a" pp header
end

module Contents = struct
  type t = unit -> string option

  let read ic header =
    In_channel.really_input_string ic
      (Int64.to_int header.Header.content_length)

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
  match contents with
  | Some contents ->
      let length = String.length contents |> Int64.of_int in
      assert (Int64.equal header.Header.content_length length);
      Out_channel.output_string oc contents;
      Out_channel.output_string oc "\r\n\r\n\r\n";
      Out_channel.flush oc
  | None -> ()

let write_all oc items = List.iter (fun (hdr, c) -> write oc hdr c) items
