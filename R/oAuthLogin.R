#Necessary Libraries
library(httr)
library(xml2)

### Define Functions
d2Session <- R6::R6Class("d2Session",
                         #' @title d2Session
                         public = list(
                           #' @field  base_url The URL of the server,
                           #' e.g. https://www.datim.org/.
                           base_url = NULL,
                           #' @field  username Your user name.
                           username = NULL,
                           #' @field user_orgunit UID of the users assigned
                           #' organisation unit
                           user_orgunit = NULL,
                           #' @field handle An httr handle used to communicate
                           #' with the DHIS2 instance.
                           handle = NULL,
                           #' @field me dhis2 api/me response
                           me  = NULL,
                           max_cache_age  = NULL,
                           #' @description
                           #' Create a new DHISLogin object
                           #' @param base_url URL to the server.
                           #' @param handle httr handle to be used for dhis2
                           #' connections
                           #' @param me DHIS2 me response object
                           #' @param max_cache_age cache expiry currently used
                           #' by datim validation
                           initialize = function(base_url,
                                                 handle,
                                                 me) {
                             self$me <- me
                             self$user_orgunit <- me$organisationUnits$id
                             self$base_url <- base_url
                             self$username <- me$userCredentials$username
                             self$handle <- handle
                           }
                         )
)

#' @title getOAuthToken (app_url,app,api,scope)
#' @description retrieve authorization token from DATIM/DHIS2
#' @param app OAuth2 client details - Name, Client ID, Client Secret,
#' redirect URIs
#' @param redirect_uri Redirect_uri listed under client details
#' @param api Endpoint for authorization - base url, authorize endpoint, access
#' token endpoint
#' @param scope the scope of what the client will return. "All" is default for
#' DHIS2
#' @return access token to specified OAuth2 Client
#'
getOAuthToken <- function(redirect_uri, app, api, scope) {

  token <- oauth2.0_token(
    app = app,
    endpoint = api,
    scope = scope,
    use_basic_auth = TRUE,
    oob_value = redirect_uri,
    cache = FALSE
  )

  return(token)
}

# TODO Need a better user input here that pops up on the screen as opposed
# to terminal

###############################################################################
#' @export
#' @title loginToDATIMOAuth()
#' @description login to a datim or dhis2 api using Oauth2.
#' This function creates a d2Session login object in the
#' environment calling the login function.
#' E.g. global environment or R-shiny session. Thus you do not need to assign
#' the output of this function to a variable as it creates the variable/object
#' as a side effect.
#' @param base_url The base url for the instance you are authenticating against.
#' @param token The authorization token granted after using getOAuthToken()
#' @param redirect_uri Redirect_uri listed under client details
#' @param app OAuth2 client details - Name, Client ID, Client Secret,
#' redirect URIs
#' @param api Endpoint for authorization - base url, authorize endpoint,
#' access token endpoint
#' @param scope The scope of what the client will return. "All" is default for
#' DHIS2
#' @param d2_session_name the variable name for the d2Session object. The
#' default name is d2_default_session and will be used by other datimutils
#' functions by default when connecting to datim. Generally a custom name
#' should only be needed if you need to log into two seperate DHIS2 instances
#' at the same time. If you create a d2Session object with a custom name then
#' this object must be passed to other datimutils functions explicitly
#' @param d2_session_envir the environment in which to place the R6 login
#' object, default is the immediate calling environment
loginToDATIMOAuth <- function(
                         base_url = NULL,
                         token = NULL,
                         redirect_uri= NULL,
                         app= NULL,
                         api= NULL,
                         scope= NULL,
                         d2_session_name = "d2_default_session",
                         d2_session_envir = parent.frame()) {

  if (is.null(token)) {
    token <- getOAuthToken(redirect_uri, app, api, scope)
  } else {
    token <- token #For Shiny
  }

  # form url
  url <- utils::URLencode(URL = paste0(base_url, "api", "/me"))
  handle <- httr::handle(base_url)

  #Get Request
  r <- httr::GET(
    url,
    config(token = token),
    httr::timeout(60),
    handle = handle
  )
  if (r$status != 200L) {
    stop("Could not authenticate you with the server!")
  } else {
    me <- jsonlite::fromJSON(httr::content(r, as = "text")) 
    
    
    # create the session object in the calling environment of the login function
    assign(d2_session_name, 
           d2Session$new(base_url = base_url,
                         handle = handle,
                         me = me), 
           envir = d2_session_envir)
  }
  
  
}



### Example of using the above.
# {
# ### Define variables
# #
# redirect_uri <- "http://127.0.0.1:8100/"
# 
# # Copy information directly from the dhis2.org web settings
# app <- oauth_app("OAuth2 Demo Client", #dhis2 = Name
#                  key = "demo",         #dhis2 = Client ID
#                  secret =
#                "76fd0224e-923c-fc67-151a-7619de5c5f7", #dhis2 = Client Secret
#                  redirect_uri = redirect_uri #"http://127.0.0.1:8100/"
# )
# 
# ## Endpoint details
# api <- oauth_endpoint(base_url = "https://play.dhis2.org/2.36.4/uaa/oauth",
#                       request=NULL,#Documentation says to leave this NULL
#                       #for OAuth2
#                       authorize = "authorize",
#                       access="token"
# )
# 
# ## Scope of what is to be returned
# scope <- "ALL"
# ## Set options
# options(httr_oob_default=TRUE)
# 
# ## Use function created above
# loginToDATIMOAuth(base_url = "play.dhis2.org/2.36.4/",app=app, api = api,
# redirect_uri=redirect_uri,scope = scope)
# 
# ## Try to query the API using the D2Session created above.
# data <- getMetadata(
#   end_point = "organisationUnits",
#   "organisationUnitGroups.name:eq:District",
#   fields = "id,name,level"
# )
# head(data)
# }
