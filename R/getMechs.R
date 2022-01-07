#' @export
#' @title getMechs(username=NULL, password = NULL)
#' @description Allows developer to derive datim user type and apply security restrictions accordingly. 
#' @param username DHIS 2 username.
#' @param password DHIS 2 password for the username.
#' @param base_url base_url The URL of the server, e.g. https://www.datim.org/. 
#' @param by the id mechanisms should be pulled by, e.g. cocuid, mech_id, mech_name.

getMechs <- function(username = NULL, password = NULL, base_url = NULL, by="cocuid") {
  
  #stop if username and password are not entered
  if((!(is.null(username)) && is.null(password)) || (is.null(username) && !(is.null(password)))){
    stop("If directly providing function credentials you must specify both username and password")
  }
  
  # fetch api, report error if unable to resolve
  url <- paste0(base_url,"api/categoryOptions?filter=categories.id:eq:SH885jaRe0o&fields=name,id,categoryOptionCombos[name,id,code]&paging=false")
  tryCatch(
    expr = {
      req <- httr::GET(url, httr::authenticate(username, password))
    },
    error = function(e){ 
      print(e)
    }
  )
  
  # process api content
  json <- httr::content(req)
  col_names <- c("mech_id", "category_option_combos_id", "name")
  my_cat_ops <- unstack(data.frame(d<-unlist(json),names(d)))[,c(1,2,3)]
  names(my_cat_ops) <- col_names
  
  #process json differently depending on the return value desired
  if (by=="cocuid") {
    
    #return by category option combo id
    return(my_cat_ops[,c("category_option_combos_id", "name")])
    
  } else if (by == "mech_id") {
    
    #return by mech number
    return(my_cat_ops[,c("mech_id", "name")])
    
  } else if (by == "mech_name")  {
    
    #return by mech name
    return(my_cat_ops[,c("name"), drop=FALSE])
    
  } else {
    
    return(my_cat_ops)
  }
}
