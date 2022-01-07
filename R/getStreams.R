#' @export
#' @title getStreams(username=NULL, password = NULL)
#' @description Allows developer to derive datim user streams and apply security restrictions accordingly. 
#' @param username DHIS 2 username.
#' @param password DHIS 2 password for the username.
#' @param base_url base_url The URL of the server, e.g. https://www.datim.org/. 

getStreams <- function(username = NULL, password = NULL, base_url = NULL) {
  
  #stop if username and password are not entered
  if((!(is.null(username)) && is.null(password)) || (is.null(username) && !(is.null(password)))){
    stop("If directly providing function credentials you must specify both username and password")
  }
  
  # fetch api, report error if unable to resolve
  url <- paste0(base_url,"api/me?fields=userGroups%5Bname,id%5D")
  tryCatch(
    expr = {
      req <- httr::GET(url, httr::authenticate(username, password))
    },
    error = function(e){ 
      print(e)
    }
  )
  
  #process group ids
  json <- httr::content(req, "text")
  groups_id <- jsonlite::fromJSON(json)$userGroups[,"name"]
  
  #eliminate data and access elements
  groups_id <- gsub("Data ", "", groups_id)
  groups_id <- gsub(" access", "", groups_id)
  
  #filter out Global and Dev streams
  #groups_id <- groups_id[!grepl("Dev|Global",groups_id)]
  
  #return data frame
  groups_id_df <- data.frame(stream = groups_id)
  
  return(groups_id_df)
}
