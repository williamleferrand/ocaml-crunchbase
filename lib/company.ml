type t = 
    {
      (* Administrative *)
      name : string ; 
      permalink : string ; 

      (* Contact *)
      homepage_url : string ; 
      blog_url : string ; 
      blog_feed_url : string ;
      twitter_username : string option ;
      
      (* General info *)
      category_code : string ; 
      number_of_employees : int option ; 
            
      founded_year : int ; 
      founded_month : int option ; 
      founded_day : int option ; 

      tag_list : string list ; 
      email_address : string ;
      phone_number : string ; 

      description : string option ; 
      overview : string ; (* utf8 encoded, remember *)
      
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
        
        homepage_url = get_string values "homepage_url" ;
        blog_url = get_string values "blog_url" ;
        blog_feed_url = get_string values "blog_feed_url" ; 
        twitter_username = get_string_option values "twitter_username" ;

        category_code = get_string values "category_code" ;
        number_of_employees = get_int_option values "number_of_employees" ;
        
        founded_year = get_int values "founded_year" ;
        founded_month = get_int_option values "founded_month" ; 
        founded_day = get_int_option values "founded_day" ;

        tag_list = get_string_list' values "tag_list" ;
        email_address = get_string values "email_address" ; 
        phone_number = get_string values "phone_number" ; 

        description = get_string_option values "description" ; 
        overview = get_string values "overview" ; 

        competitions = get_raw_list values Competition.of_json "competitions" ; 
        
        funding_rounds = get_raw_list values Round.of_json "funding_rounds" ; 
        acquisition = None ;

        offices = get_raw_list values Office.of_json "offices" ;
        ipo = None ;
      }
