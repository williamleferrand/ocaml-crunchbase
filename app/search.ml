open Lwt

(* get all the companies with some funding *)

let rec iter ?(page=1) acc query = 
  Api.search ~page query 
  >>= function 
    | [] -> return acc
    | _ as l ->
      Lwt_list.fold_left_s
        (fun acc result -> 
          match result.Api.namespace with 
              `Company -> 
                (let permalink = result.Api.permalink in
                 Printf.printf "."; flush stdout ;
                 Api.company permalink
                 >>= fun company -> 
                 match company.Company.funding_rounds with 
                     [] -> return acc 
                   | _ -> return (company :: acc))
            | _ -> return acc) acc l 
      >>= fun nacc -> 
      Printf.printf "\n"; flush stdout ;
      iter ~page:(page+1) nacc query

let rec iter_p ?(page=1) acc query = 
  catch 
    (fun () -> 
      Api.search ~page query 
      >>= function 
        | [] -> return acc
        | _ as l ->
          Lwt_list.map_p 
            (fun result -> 
              match result.Api.namespace with 
              `Company -> 
                (let permalink = result.Api.permalink in
                 Printf.printf "."; flush stdout ;
                 Api.company permalink
                 >>= fun company -> 
                 match company.Company.funding_rounds with 
                     [] -> return None
                   | _ -> return (Some company))
                | _ -> return None) l 
          >>= fun cps -> 
          let l = List.fold_left 
            (fun acc r -> 
              match r with 
                  None -> acc
                | Some c -> c :: acc)
            acc cps in
          iter_p ~page:(page+1) l query)
    (function 
      | End_of_file -> Printf.printf "\n"; return acc
      | _ as e -> fail e)

let search query = 
  print_endline ">> Crunchbase API reader" ;
  Lwt_main.run 
    (iter_p [] Sys.argv.(1) 
     >>= fun r -> List.iter (fun r -> print_endline r.Company.name) (List.rev r); return ())

(* Search with filters **)

let filter1 file = 
  Lwt_io.open_file ~mode:Lwt_io.output file
  >>= fun oc -> 
  return (fun company ->
    Printf.printf "." ; flush stdout ; 
      let open Company in
          match company.tag_list with 
              None -> return () 
            | Some tags -> 
              match List.mem "sport" tags || List.mem "sports" tags with
                  false -> return ()
                | true -> 
                  match company.funding_rounds with 
                      [] -> return ()
                 | _ -> Lwt_io.write_line oc company.permalink)

let filter2 file = 
  Lwt_io.open_file ~mode:Lwt_io.output file
  >>= fun oc -> 
  return (fun company ->
    Printf.printf "." ; flush stdout ; 
    let open Company in
          match company.tag_list with 
              None -> return () 
            | Some tags -> 
              match List.mem "sport" tags || List.mem "sports" tags with
                  false -> return ()
                | true -> 
                  match company.funding_rounds with 
                      [] -> return ()
                    | _ -> 
                      Lwt_io.write_line oc (Company.to_csv company))
let _ = 
  Lwt_main.run 
    (
      filter1 Sys.argv.(2) 
      >>= fun filter -> Api.select_company_from_file Sys.argv.(1) filter
     >>= fun l ->
     Printf.printf "%d companies match\n" (List.length l); 
     return ())

    
