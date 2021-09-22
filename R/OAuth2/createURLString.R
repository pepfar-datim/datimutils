#' Assembles a body for a post request to retreive the user's token.
#' @param key your personal api key
#' @param redirect_uri your apps redirect URL (aka where you app lives on the internet or localhost)
#' @param auth_code Code that was retreived from an earlier authorization request client-side.
#' @return A string formatted to be plopped into a \code(httr::POST) request.
#' @export
#' @examples
#' makeBodyString(key= "fhauh108', redirect_uri = "localhost:1410", authcode = "08oiweifoaoihfoijfaodjf")

createURLString <- function(key, redirect_uri, auth_code){
  sprintf("client_id=%s&grant_type=authorization_code&redirect_uri=%s&code=%s", key, redirect_uri, auth_code)
  
  # url <- oauth2.0_authorize_url(api, app, scope = scope)
  # redirect <- sprintf("location.replace(\"%s\");", url)
  # tags$script(HTML(redirect))
}