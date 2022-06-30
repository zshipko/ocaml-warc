let () =
  let filename = Sys.argv.(1) in
  let oc = Out_channel.open_text "test.warc" in
  In_channel.with_open_text filename (fun ic ->
      let f = Warc.read_all ic in
      List.iter
        (fun (r, content) ->
          print_endline "--- Begin";
          print_endline r.Warc.Header.version;
          print_endline r.id;
          print_endline "--- Content";
          let content = content () in
          Option.iter print_endline content;
          print_endline "--- End";
          Option.iter (Warc.write oc r) content)
        f);
  Out_channel.close oc