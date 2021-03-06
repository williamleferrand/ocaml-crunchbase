type t = 
    {
      round_code : string option ;
      source_url : string option ;
      source_description : string option; 
      raised_amount : float option ;
      raised_currency_code : string option ; 
      funded_year : int option; 
      funded_month : int option ; 
      funded_day : int option ; 
      investments : Investment.t list ;
    }

let of_json json = 
   let open Util in 
      let values = read_json json in 
      {
        round_code = get_string_option values "round_code" ;
        source_url = get_string_option values "source_url" ;
        source_description = get_string_option values "source_description" ; 
        raised_amount = get_float_option values "raised_amount" ; 
        raised_currency_code = get_string_option values "raised_currency_code" ;
        funded_year = get_int_option values "funded_year" ; 
        funded_month = get_int_option values "funded_month" ;
        funded_day = get_int_option values "funded_day" ;
        investments = get_raw_list values Investment.of_json "investments"
      }


let sp = function Some s -> s | None -> "" 
let spi = function Some i -> string_of_int i | None -> "" 
let spf = function Some i -> string_of_float i | None -> "" 
let spl = function Some l -> String.concat " " l | None -> "" 

let to_csv t = 
  Printf.sprintf "%s, %s, %s, %s, %s, %s, %s, %s"
    (sp t.round_code)
    (sp t.source_url)
    (sp t.source_description)
    (spf t.raised_amount)
    (sp t.raised_currency_code)
    (spi t.funded_year)
    (spi t.funded_month)
    (spi t.funded_day)

