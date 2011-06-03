exception Malformed
exception Missing of string 

let read_json = function 
  | `Assoc l -> 
    let values = Hashtbl.create 0 in
    List.iter (fun (n, v) -> Hashtbl.add values n v) l ; 
    values 
  | _ -> raise Malformed 

let get_string values name = 
  try
    match Hashtbl.find values name with 
      | `String s -> s 
      | _ -> raise (Missing name)
  with Not_found -> raise (Missing name)

let get_string_option values name = 
  try 
   match Hashtbl.find values name with 
      | `String s -> Some s 
      | _ -> None
  with Not_found -> None
    
let get_float values name = 
  try
    match Hashtbl.find values name with 
      | `Float f -> f 
      | _ -> raise (Missing name)
  with Not_found -> raise (Missing name)

let get_string_list values name = 
  try
    match Hashtbl.find values name with 
      | `List l -> List.map (function `String s -> s | _ -> raise (Missing name)) l
      | _ -> raise (Missing name)
  with Not_found -> raise (Missing name)

(* raw getters *)

let get_raw_list values op name = 
  try 
    match Hashtbl.find values name with 
      | `List l -> List.map op l
      | _ -> raise (Missing name)
  with Not_found -> raise (Missing name)

let get_raw values op name = 
  try 
    op (Hashtbl.find values name)
  with Not_found -> raise (Missing name)

let get_raw_option values op name = 
  try 
    Some (op (Hashtbl.find values name))
  with Not_found -> None

let get_int values name = 
  try 
    match Hashtbl.find values name with 
        `Int i -> i
      | _ -> raise (Missing name)
  with Not_found -> raise (Missing name)

let get_int_option values name = 
  try 
   match Hashtbl.find values name with 
      | `Int i -> Some i 
      | _ -> None
  with Not_found -> None
    
