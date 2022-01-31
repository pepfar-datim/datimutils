#' @export
#' @title getMyUserType(streams = NULL)
#' @description Returns a classified user type based on accessible data streams. 
#' @streams data streams as a character vector.
#' @param d2_session the d2Session object, default is "d2_default_session"

getMyUserType <- function(streams = NULL, d2_session = dynGet("d2_default_session",
                                                              inherits = TRUE)) {
  
  # if there is no input default the d2 object
  if(is.null(streams)) {
    
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
    
  # if a user provides streams make sure they are input as a character vector 
  } else if (!is.character(streams)) {
    stop("What you provided is not a character vector. Please provide a character vector of streams.")
  
  # otherwise classify the streams they provided  
  } else {
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
}



