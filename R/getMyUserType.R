#' @export
#' @title getMyUserType(streams = NULL)
#' @description Returns a classified user type based on accessible data streams. 
#' @streams data streams as a character vector.

getMyUserType <- function(streams = NULL) {
  
  if(is.null(streams)) {
    stop("You must provide a vector of streams to classify users")
  } else if (!is.character(streams)) {
    stop("What you provided is not a character vector. Please provide a character vector of streams.")
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



