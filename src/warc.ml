module Record = struct
  type warctype =
    | Warcinfo
    | Response
    | Resource
    | Request
    | Metadata
    | Revisit
    | Conversion
    | Continuation

  type t = { id: string; content_length: int; date: string; warctype : warctype }
  
end
