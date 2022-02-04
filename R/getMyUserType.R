#' @export
#' @title getMyUserType(d2_session = dynGet("d2_default_session", inherits = TRUE))
#' @description Returns a classified user type based on accessible data streams. 
#' @param d2_session the d2Session object, default is "d2_default_session"

getMyUserType <- function(d2_session = dynGet("d2_default_session",inherits = TRUE)) {
  
  #pull streams and classify
  tryCatch(
    expr = {
      user_groups <- getUserGroups(
        values = d2_session$me$userGroups$id,
        d2_session = d2_session
      )
    },
    error = function(e){ 
      print(e)
    }
  )
  
  # select from user_groups everything that is a data stream and needed for classification
  streams <- user_groups[grepl("Data (.+?) access|^Global|^OU",user_groups)]
    
    # classify user
    if( length( regmatches(streams ,regexpr("OU (.+?) MOH users", streams)) )  > 0 ) {
      return("MOH")
    } else if ( length(regmatches(streams ,regexpr("Global users", streams)) ) > 0 ) {
      return("Global")
    } else if ( length( regmatches(streams, regexpr("OU (.+?) Partner (.+?) users - (.+?)",streams)) ) > 0 ) {
      return("Partner")
    } else if ( length( regmatches(streams, regexpr("OU (.+?) Agency (.+?) users",streams)) ) > 0 ) {
      return("Agency")
    } else if ( length( regmatches(streams, regexpr("OU (.+?) Interagency users",streams)) ) > 0 ) {
      return("Interagency")
    } else if ( length( regmatches(streams, regexpr("Global Agency (.+?) users",streams)) ) > 0 ) {
      return("Global Agency")
    } else if ( length( regmatches(streams, regexpr("Global Partner (.+?) users - (.+?)",streams)) ) > 0 ) {
      return("Global Partner")
    } else {
      return("Unclassified user")
    }
}



