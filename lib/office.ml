exception Malformed 

type t = 
    {
      description : string option ; 
      address1 : string ; 
      address2 : string ; 
      zip_code : string ; 
      city : string ; 
      state_code : string ; 
      country_code : string ; 
      latitude : string option ; 
      longitude : string option ;
    }


let of_json json = 
  let open Util in 
      let values = read_json json in 
      {
        description = get_string_option values "description" ; 
        address1 = get_string values "address1" ;
        address2 = get_string values "address2" ;
        zip_code = get_string values "zip_code" ; 
        city = get_string values "city" ; 
        state_code = get_string values "state_code" ; 
        country_code = get_string values "country_code" ;
        latitude = get_string_option values "latitude" ;
        longitude = get_string_option values "longitude" ;
      }
  
