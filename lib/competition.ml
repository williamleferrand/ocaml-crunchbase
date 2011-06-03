type competitor = 
    {
      name : string ; 
      permalink : string ;
    }

let competitor_of_json json = 
  let open Util in 
      let values = read_json json in 
      {
        name = get_string values "name" ;
        permalink = get_string values "permalink" }


type t = [ `Competitor of competitor ]

let of_json json =
  let open Util in 
      match json with 
          `Assoc 
            [
              "competitor", competitor 
            ] -> `Competitor (competitor_of_json competitor)
        | _ -> raise Malformed 
