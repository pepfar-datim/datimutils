#' @export
#' @title getMyUserType(d2_session = dynGet("d2_default_session", inherits = TRUE))
#' @description Returns a classified user type based on accessible data streams. 
#' @param d2_session the d2Session object, default is "d2_default_session"

getMyUserType <- function(d2_session = dynGet("d2_default_session",inherits = TRUE)) {
  
    #pull streams and classify
    streams <- getMyStreams(d2_session = d2_session)
    
    #classify user
    if( length( regmatches(streams ,regexpr("OU (.+?) MOH users", streams)) )  > 0 ) {
      return("moh user")
    } else if ( length(regmatches(streams ,regexpr("Global users", streams)) ) > 0 ) {
      return("global only")
    } else if ( length( regmatches(streams, regexpr("OU (.+?) Partner (.+?) users - (.+?)",streams)) ) > 0 ) {
      return("partner only")
    } else if ( length( regmatches(streams, regexpr("OU (.+?) Agency (.+?) users",streams)) ) > 0 ) {
      return("agency only")
    } else if ( length( regmatches(streams, regexpr("OU (.+?) Interagency users",streams)) ) > 0 ) {
      return("interagency only")
    } else if ( length( regmatches(streams, regexpr("Global Agency (.+?) users",streams)) ) > 0 ) {
      return("global agency")
    } else if ( length( regmatches(streams, regexpr("Global Partner (.+?) users - (.+?)",streams)) ) > 0 ) {
      return("global partner")
    } else {
      return("unclassified user")
    }
}



