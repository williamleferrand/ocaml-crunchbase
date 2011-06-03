open Lwt
open Crunchbase


let rec get_all ?(page=1) acc query = 
  Crunchbase.search ~page query 
  >>= function 
    | [] -> return acc
    | _ as l -> get_all ~page:(page+1) (acc @ l) query

let _ =
  print_endline ">> Crunchbase API reader" ;
  Lwt_main.run 
    (
      Crunchbase.search "sport")
