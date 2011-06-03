type t = 
    {
      round_code : string ;
      source_url : string ;
      source_description : string ; 
      raised_amount : float ;
      raised_currency_code : string ; 
      funded_year : string; 
      funded_month : string ; 
      funded_day : string ; 
      investments : Investment.t list ;
    }

let of_json json = 
   let open Util in 
      let values = read_json json in 
      {
        round_code = get_string values "round_code" ;
        source_url = get_string values "source_url" ;
        source_description = get_string values "source_description" ; 
        raised_amount = get_float values "raised_amount" ; 
        raised_currency_code = get_string values "raised_currency_code" ;
        funded_year = get_string values "funded_year" ; 
        funded_month = get_string values "funded_month" ;
        funded_day = get_string values "funded_day" ;
        investments = get_raw_list values Investment.of_json "investments"
      }
