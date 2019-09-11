#' @export
#' @importFrom utils URLencode
#' @importFrom jsonlite fromJSON
#' @import httr
#' @description Login to a DHIS 2 play environment
#' @param string - version of play DHIS2 to use e.g. "2.30"
#' @return boolean TRUE if log in succesful 

DHISLogin_Play<-function(version) {
  url <- URLencode(URL = paste0("https://play.dhis2.org/", version, "/api/me"))
  #Logging in here will give us a cookie to reuse
  r <- httr::GET(url ,
                 httr::authenticate("admin", "district"),
                 httr::timeout(60))
  if(r$status != 200L){
    stop("Could not authenticate you with the server!")
  } else {
    me <- jsonlite::fromJSON(httr::content(r,vas = "text"))
    return(TRUE)
  }
}