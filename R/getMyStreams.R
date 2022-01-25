#' @export
#' @title getMyStreams(d2_session = dynGet("d2_default_session", inherits = TRUE))
#' @description Returns datim user streams. 
#' @param d2_session the d2Session object, default is "d2_default_session",


getMyStreams <- function(d2_session = dynGet("d2_default_session", inherits = TRUE)) {
  
  # pull all user groups streams
  tryCatch(
    expr = {
      user_groups <- d2_default_session$me$userGroups$id %>% getUserGroups() 
    },
    error = function(e){ 
      print(e)
    }
  )
  
  # select from user_groups everything that is a data stream and needed for classification
  user_groups <- user_groups[grepl("Data (.+?) access|^Global|^OU",user_groups)]
  
  # remove Data Access strings
  streams <- gsub("Data | access", "", user_groups)
  
  return(streams)
}
