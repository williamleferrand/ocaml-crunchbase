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
        | Http_client.Http_error _ -> print_endline "Error" ; fail e
        | _ -> fail e) 

(* Per-item get API ************************************************************************************)

(* http://api.crunchbase.com/v/1/company/play-hard-sports.js *)

let company_base_url = "http://api.crunchbase.com/v/1/company"

let company permalink = 
  let url = Printf.sprintf "%s/%s.js" company_base_url permalink in 
  catch 
    (fun () -> 
      Http_client.get url 
      >>= fun (_, s) -> 
      print_endline s; 
      let json = Yojson.Basic.from_string s in
      let company = Company.of_json json in
      return company)
    (fun e -> 
      match e with 
        | Http_client.Http_error _ -> print_endline "Error" ; fail e
        | _ -> fail e) 



(* Select API ******************************************************************************************)

let select_company_url = "http://api.crunchbase.com/v/1/companies.js"

let permalinks_of_json = function 
  | `List l -> 
    List.map 
      (function 
      `Assoc [ "name" , `String name ; 
                   "permalink", `String permalink ] -> permalink 
        | _ -> raise Util.Malformed) l
  | _ -> raise Util.Malformed

let select_company filter = 
  Http_client.get select_company_url 
  >>= fun (_, s) -> 
  let json = Yojson.Basic.from_string s in
  let permalinks = permalinks_of_json json in
  Lwt_list.map_p
    (fun permalink ->
      company permalink
      >>= fun company -> 
      match filter company with 
          true -> return (Some company) 
        | false -> return None) permalinks 
      
