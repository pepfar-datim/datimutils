#' @export
#' @title DHISLogin_Play
#' @description Login to a DHIS 2 play environment
#' @param version string - version of play DHIS2 to use e.g. "2.30". Defaults to current datim 
#' version in production
#' @return boolean TRUE if log in succesful 

DHISLogin_Play <- function(version = "2.30") {
  url <- utils::URLencode(URL = paste0("https://play.dhis2.org/", version, "/api/me"))
  #Logging in here will give us a cookie to reuse
  r <- httr::GET(url ,
                 httr::authenticate("admin", "district"),
                 httr::timeout(60))
  if(r$status != 200L){
    stop("Could not authenticate you with the server!")
  } else {
    me <- jsonlite::fromJSON(httr::content(r, as = "text"))
    return(TRUE)
  }
}
