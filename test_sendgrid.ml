open Printf

let (>>=) = Lwt.bind

let print_success msg b =
  printf "%s: %s\n%!" msg (if b then "OK" else "ERROR");
  Lwt.return ()

let random_id =
  Random.self_init ();
  fun () -> sprintf "%08x" (Random.int (1 lsl 30 - 1))

let test1 email_addr () =
  let credentials = Keys.get () in
  let id = random_id () in
  let api_key =
    match credentials.Keys_t.sendgrid_key with
        None -> failwith "Missing Sendgrid key"
      | Some x -> x
  in
  Sendgrid.send_mail
    ~api_user: credentials.Keys_t.sendgrid_user
    ~api_key
    ~from: email_addr
    ~fromname: "Tester Sending"
    ~to_: email_addr
    ~toname: "Tester Receiving"
    ~subject: (sprintf "Welcome! [test %s]" id)
    ~text: "Welcome to SendGrid!"
    ~html: "<html><body><p>Welcome to <b>SendGrid</b>!</p></body></html>"
    ~category: ["test"]
    ()
    >>= print_success (sprintf "test1 [%s]" id)

let main ~offset =
  Log.level := `Debug;
  let email_addr =
    match Cmdline.parse
      ~offset
      ~anon_count:1
      ~usage_msg:"Usage: test_sendgrid EMAIL" []
    with
        [s] -> s
      | _ -> assert false
  in
  let tests = List.map (fun f -> f ()) [
    test1 email_addr
  ]
  in
  Lwt_main.run (Lwt.join tests)
