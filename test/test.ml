let header = Alcotest.testable Warc.Header.pp Warc.Header.equal

let test_read_write () =
  let a =
    In_channel.with_open_bin "example.warc" (fun ic ->
        let w = Warc.read_all ic in
        let w = List.map (fun (k, v) -> (k, v ())) w in
        Out_channel.with_open_bin "test.warc" (fun oc -> Warc.write_all oc w);
        w)
  in
  In_channel.with_open_bin "example.warc" (fun ic ->
      let b = Warc.read_all ic in
      let b = List.map (fun (k, v) -> (k, v ())) b in
      Alcotest.(check (list (pair header (option string)))) "equal" a b)

let () =
  let open Alcotest in 
  run "WARC" [
    "io", [test_case "Read/Write WARC file" `Quick test_read_write]
  ]