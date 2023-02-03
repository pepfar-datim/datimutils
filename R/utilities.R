#' @export
#' @title validate_base_url(base_url)
#' @description Validates that the base URL is in the required format.
#' @param d2_session the d2Session object, default is "d2_default_session"
validate_base_url <- function(base_url = NULL) {
  if (is.null(base_url)) {
    return(FALSE)
  }

  if (length(base_url) > 1) {
    return(FALSE)
  }
  #Check the form of the base_url. Note that this is
  #not meant to be a complete validation of the URL.
  base_url_pattern <- "https?:\\/\\/.+/$"
  grepl(base_url_pattern, base_url)
}
