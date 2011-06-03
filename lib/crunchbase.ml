(* crunchbase api *)

open Lwt 
open Cohttp

(* Search API ********************************************************************************************)

exception Malformed 

type r =
    {
      name : string ; 
      permalink : string ;
      crunchbase_url : string ; 
    }
      
let base_url = "http://api.crunchbase.com/v/1/search.js"

let result_of_json = function 
  | `Assoc
      (
        ("name", `String name) 
        :: ("permalink", `String permalink)
        :: ("crunchbase_url", `String crunchbase_url)
        :: _)  
      -> { name; permalink; crunchbase_url }
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
  let url = Printf.sprintf "%s?%s" base_url params in
  
  catch 
    (fun () -> 
      Http_client.get url 
      >>= fun (_, s) -> 
      let results = search_result_of_string s in 
      List.iter (fun r -> print_endline r.name) results; 
      return results)
    (fun e ->
      match e with 
        | Http_client.Http_error _ -> print_endline "Error" ; fail e
        | _ -> fail e) 


