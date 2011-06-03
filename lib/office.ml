exception Malformed 

type t = 
    {
      description : string option ; 
      address1 : string option ; 
      address2 : string option ; 
      zip_code : string option ; 
      city : string option ; 
      state_code : string option ; 
      country_code : string option ; 
      latitude : string option ; 
      longitude : string option ;
    }

let of_json json = 
  let open Util in 
      let values = read_json json in 
      {
        description = get_string_option values "description" ; 
        address1 = get_string_option values "address1" ;
        address2 = get_string_option values "address2" ;
        zip_code = get_string_option values "zip_code" ; 
        city = get_string_option values "city" ; 
        state_code = get_string_option values "state_code" ; 
        country_code = get_string_option values "country_code" ;
        latitude = get_string_option values "latitude" ;
        longitude = get_string_option values "longitude" ;
      }
  
