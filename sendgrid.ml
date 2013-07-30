(*
  See API specification at
  http://docs.sendgrid.com/documentation/api/web-api
*)

open Printf
open Log

open Sendgrid_t

let base_url = "https://sendgrid.com/api/"
let mail_send_url = Uri.of_string (base_url ^ "mail.send.json")

(* Convert JSON object to application/x-www-form-urlencoded *)
let form_data_of_json s =
  let j = Yojson.Basic.from_string s in
  let kv_list =
    match j with
        `Assoc l ->
          List.map (
            function
              | k, `String s -> (k, s)
              | k, x -> (k, Yojson.Basic.to_string x)
          ) l
      | _ -> assert false
  in
  Nlencoding.Url.mk_url_encoded_parameters kv_list

let handle_response http_resp =
  let opt_resp, opt_status =
    match http_resp with
      | Some (status, headers, body) ->
        (try
           Some (Sendgrid_j.response_of_string body), Some status
         with _ ->
           logf `Error "Unable to parse sendgrid's JSON response: %S" body;
           None, None
        )
      | None -> None, None
  in

  let is_success =
    match opt_status with
      | Some status ->
        (match status with
          | `OK ->
            (match opt_resp with
                Some { message = `Success; _ } -> true
              | Some { message = `Error; errors } ->
                logf `Error "sendgrid soft error:\n%s"
                  (String.concat "\n" errors);
                false
              | None -> false
            )

          | #Cohttp.Code.client_error_status ->
            logf `Critical
              "sendgrid response status (client error): %s"
              (Cohttp.Code.string_of_status status);
            false

          | #Cohttp.Code.server_error_status ->
            logf `Error
              "sendgrid response status (server error): %s"
              (Cohttp.Code.string_of_status status);
            false

          | #Cohttp.Code.success_status
          | #Cohttp.Code.redirection_status
          | #Cohttp.Code.informational_status ->
            logf `Error "sendgrid response status (other error): %s"
              (Cohttp.Code.string_of_status status);
            false

          | `Code n ->
            logf `Error "sendgrid response status (unknown): %i" n;
            false
        )
      | None ->
        logf `Error "no HTTP response from sendgrid";
        false
  in
  Lwt.return is_success

(* We do this ourselves because otherwise Sendgrid does it for us, poorly. *)
let html_of_text s =
  sprintf "<html><body><pre>%s</pre></body></html>" (Util_html.encode s)

let single_lf = Pcre.regexp "(?<!\r)\n"

(* Use CRLF for newlines in text body
   http://tools.ietf.org/html/rfc2046#section-4.1.1 *)
let fix_newlines s =
  Pcre.replace ~rex:single_lf ~templ:"\r\n" s

let send_mail
    ~api_user
    ~api_key
    ~from
    ?fromname
    ~to_
    ?toname
    ?bcc
    ~subject
    ?text
    ?html
    ~category
    ?date
    () =

  let date =
    match date with
        None -> Unix.gettimeofday ()
      | Some t -> t
  in

  let html =
    match html with
      | Some _ as x -> x
      | None ->
          match text with
            | None -> None
            | Some s -> Some (html_of_text s)
  in

  let req =
    Sendgrid_v.create_mail_send_request
      ~api_user
      ~api_key
      ~x_smtpapi: (Sendgrid_v.create_x_smtpapi_header ~category ())
      ~to_
      ?toname
      ~subject
      ?text: (BatOption.map fix_newlines text)
      ?html
      ~from
      ?fromname
      ?bcc
      ~date: (Nldate.mk_mail_date ~localzone:true date (* timezone = "GMT" *))
      ()
  in
  match Sendgrid_v.validate_mail_send_request [] req with
      Some err ->
        logf `Error "Invalid sendgrid mail.send request: %s"
          (Ag_util.Validation.string_of_error err);
        Lwt.return false
    | None ->
        let json_req = Sendgrid_j.string_of_mail_send_request req in
        Log.debug (fun () -> json_req);
        let form_data = form_data_of_json json_req in
        Log.debug (fun () -> form_data);
        let headers = [
          "content-type", "application/x-www-form-urlencoded; charset=utf-8"
        ]
        in
        let req_lwt =
          Util_http_client.post
            ~headers
            ~body: form_data
            mail_send_url
        in
        Lwt.bind req_lwt handle_response
