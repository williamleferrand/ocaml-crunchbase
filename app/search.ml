open Lwt
open Crunchbase


let rec iter ?(page=1) acc query = 
  Crunchbase.search ~page query 
  >>= function 
    | [] -> return acc
    | _ as l -> iter ~page:(page+1) (acc @ l) query

let _ =
  print_endline ">> Crunchbase API reader" ;
  Lwt_main.run 
    (iter [] Sys.argv.(1) 
     >>= fun r -> List.iter (fun r -> print_endline r.name) r; return ())
