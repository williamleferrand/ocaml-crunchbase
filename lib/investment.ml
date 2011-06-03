type t = 
    {
      company : string option ; 
      financial_org : Financial_org.t option ; 
      person : string option ; 
    }

let of_json json = 
  let open Util in 
      let values = read_json json in 
      {
        company = get_string_option values "company" ; 
        financial_org = get_raw_option values Financial_org.of_json "financial_org" ; 
        person = get_string_option values "person" ;
      }
