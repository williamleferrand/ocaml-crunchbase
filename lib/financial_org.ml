type t = 
    {
      name : string ; 
      permalink : string ; 
    }

let of_json json = 
  let open Util in 
      let values = read_json json in 
      {
        name = get_string values "name" ;
        permalink = get_string values "permalink" ;
      }
