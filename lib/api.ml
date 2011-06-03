(* crunchbase api *)

open Lwt 
open Cohttp

(* Some types ********************************************************************************************)

type permalink = string 

(* Search API ********************************************************************************************)

exception Malformed 

type namespace = [ `Company | `Product | `Other ]

let namespace_of_string = function 
  | "company" -> `Company 
  | "product" -> `Product 
  | _ -> `Other

type r =
    {
      name : string ; 
      namespace : namespace ;
      permalink : permalink ;
      crunchbase_url : string ; 
    }
      
let search_base_url = "http://api.crunchbase.com/v/1/search.js"

let result_of_json = function 
  | `Assoc
      (
        ("name", `String name) 
        :: ("permalink", `String permalink)
        :: ("crunchbase_url", `String crunchbase_url)
        :: ("namespace", `String namespace)
        :: _)  
      -> { name ;
           permalink ;
           crunchbase_url ;
           namespace = namespace_of_string namespace }
  | _ -> raise Malformed 

let search_result_of_string s =
  match Yojson.Basic.from_string s with
      `Assoc
        [
          "total", total; 
          "page", page ;
          "crunchbase_url", crunchbase_url ; 
          "results", `List results 
        ] -> List.map result_of_json results
    | _ -> raise Malformed
         
let search ?(page=1) query = 
  let params = Netencoding.Url.mk_url_encoded_parameters [ "page", string_of_int page ; "query", query ] in
  let url = Printf.sprintf "%s?%s" search_base_url params in
  
  catch 
    (fun () -> 
      Http_client.get url 
      >>= fun (_, s) -> 
      let results = search_result_of_string s in 
      return results)
    (fun e ->
      match e with 
        | Http_client.Http_error _ -> Printf.printf "e" ; fail e
        | _ -> fail e) 

(* Per-item get API ************************************************************************************)

(* http://api.crunchbase.com/v/1/company/play-hard-sports.js *)

let company_base_url = "http://api.crunchbase.com/v/1/company"

let rec company ?location permalink = 
  let url = match location with Some url -> url | None -> Printf.sprintf "%s/%s.js" company_base_url permalink in 
  catch 
    (fun () -> 
      Http_client.get url 
      >>= fun (_, s) -> 
      let json = Yojson.Basic.from_string s in
      let company = Company.of_json json in
      return company)
    (fun e -> 
      match e with 
        | Http_client.Http_error (302, headers, _)  -> 
          let location = List.assoc "location" headers in 
          Printf.printf "o(%s)" permalink ; flush stdout ;
          company ~location permalink
        | Http_client.Http_error (_)  -> Printf.printf "e";  fail e
        | _ -> Printf.printf "%s" (Printexc.to_string e) ;  fail e) 



(* Select API ******************************************************************************************)

let select_company_url = "http://api.crunchbase.com/v/1/companies.js"

let pool = Lwt_pool.create 100 (fun () -> return ())

(* Ok we need a nicer way to read the string .. *)

let rec read_permalinks acc = lexer 
  | "permalink\": \"" ->  read_permalink acc lexbuf
  | eof | "" -> acc
  | _ -> read_permalinks acc lexbuf

and read_permalink acc = lexer 
    | [ 'a' - 'z' '-' '0' - '9' ]+ -> read_permalinks ((Ulexing.utf8_lexeme lexbuf) :: acc) lexbuf

let select_company filter = 
  Http_client.get select_company_url 
  >>= fun (_, s) -> 
  print_endline "we have the list"; 
  let permalinks = read_permalinks [] (Ulexing.from_utf8_string s) in 
  return permalinks

let select_company_from_file file filter = 
  let ic = open_in file in
  let lexbuf = Ulexing.from_utf8_channel ic in
  let permalinks = read_permalinks [] lexbuf in 
  close_in ic ; 
  Lwt_list.map_p
    (fun permalink ->
      Lwt_pool.use pool (fun _ -> 
        catch 
          (fun () -> company permalink >>= filter)
          (fun _ -> return ()))) permalinks 
      
