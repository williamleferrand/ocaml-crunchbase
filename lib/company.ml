type t = 
    {
      (* Administrative *)
      name : string ; 
      permalink : string ; 

      (* Contact *)
      homepage_url : string option ; 
      blog_url : string option ; 
      blog_feed_url : string option ;
      twitter_username : string option ;
      
      (* General info *)
      category_code : string option ; 
      number_of_employees : int option ; 
            
      founded_year : int option ; 
      founded_month : int option ; 
      founded_day : int option ; 

      tag_list : string list option ; 
      email_address : string option ;
      phone_number : string option ; 

      description : string option ; 
      overview : string option ; (* utf8 encoded, remember *)
      
      competitions : Competition.t list ;
      
      (* Financial data *)
      funding_rounds : Round.t list ;
      acquisition : unit option ; 
      
      offices : Office.t list ;
      ipo : unit option ;      
    }


let of_json json = 
  let open Util in 
      let values = read_json json in 
      {
        name = get_string values "name" ;
        permalink = get_string values "permalink" ;
        
        homepage_url = get_string_option values "homepage_url" ;
        blog_url = get_string_option values "blog_url" ;
        blog_feed_url = get_string_option values "blog_feed_url" ; 
        twitter_username = get_string_option values "twitter_username" ;

        category_code = get_string_option values "category_code" ;
        number_of_employees = get_int_option values "number_of_employees" ;
        
        founded_year = get_int_option values "founded_year" ;
        founded_month = get_int_option values "founded_month" ; 
        founded_day = get_int_option values "founded_day" ;

        tag_list = get_string_list_option' values "tag_list" ;
        email_address = get_string_option values "email_address" ; 
        phone_number = get_string_option values "phone_number" ; 

        description = get_string_option values "description" ; 
        overview = get_string_option values "overview" ; 

        competitions = get_raw_list values Competition.of_json "competitions" ; 
        
        funding_rounds = get_raw_list values Round.of_json "funding_rounds" ; 
        acquisition = None ;

        offices = get_raw_list values Office.of_json "offices" ;
        ipo = None ;
      }

let sp = function Some s -> s | None -> "" 
let spi = function Some i -> string_of_int i | None -> "" 
let spl = function Some l -> String.concat " " l | None -> "" 

let spr r = String.concat ", " (List.map Round.to_csv r) 

let to_csv t = 
  Printf.sprintf "%s, http://www.crunchbase.com/company/%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s"
    t.name
    t.permalink
    (sp t.homepage_url)
    (sp t.blog_url)
    (sp t.twitter_username)
    (spi t.number_of_employees)
    (spi t.founded_year)
    (spi t.founded_month)
    (spi t.founded_day)
    (spl t.tag_list)
    (sp t.email_address)
    (sp t.phone_number)
    (sp t.description)
    (spr t.funding_rounds)
    
    
    
