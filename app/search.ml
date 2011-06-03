open Lwt


let rec iter ?(page=1) acc query = 
  Api.search ~page query 
  >>= function 
    | [] -> return acc
    | _ as l -> iter ~page:(page+1) (acc @ l) query

let search query = 
  print_endline ">> Crunchbase API reader" ;
  Lwt_main.run 
    (iter [] Sys.argv.(1) 
     >>= fun r -> List.iter (fun r -> print_endline r.Api.name) r; return ())

let _ = 
  Lwt_main.run
    (Api.company Sys.argv.(1)
     >>= fun company ->
     let open Company in
         print_endline company.Company.name ; 
         return ())
