#Necessary Libraries
library(httr)
library(xml2)

### Variables used for testing
redirect_uri <- "http://127.0.0.1:8100/"

# Copy information directly from the dhis2.org web settings
app <- oauth_app("OAuth2 Demo Client", #dhis2 = Name
                 key = "demo",         #dhis2 = Client ID
                 secret = "68bd4f81d-9b0d-f256-5d9c-0234393e7ae", #dhis2 = Client Secret
                 redirect_uri = redirect_uri #"http://127.0.0.1:8100/"
)

api <- oauth_endpoint(base_url = "https://play.dhis2.org/2.36.3/uaa/oauth",
                      request=NULL,#Documentation says to leave this NULL for OAuth2 
                      authorize = "authorize",
                      access="token"
) 

scope <- "ALL"   

### Define Functions
d2Session <- R6::R6Class("d2Session",
                         #' @title d2Session 
                         public=list(
                           #' @field  config_path Path to a JSON configuration file. 
                           config_path = NULL,
                           #' @field  base_url The URL of the server, e.g. https://www.datim.org/. 
                           base_url = NULL,
                           #' @field  username Your user name. 
                           username = NULL,
                           #' @field user_orgunit UID of the users assigned organisation unit
                           user_orgunit = NULL,
                           #' @field handle An httr handle used to communicate with the DHIS2 instance. 
                           handle = NULL,
                           #' @field me dhis2 api/me response 
                           me  = NULL,
                           max_cache_age  = NULL,
                           #' @description 
                           #' Create a new DHISLogin object
                           #' @param config_path Configuration file path
                           #' @param base_url URL to the server. 
                           #' @param handle httr handle to be used for dhis2 connections 
                           #' @param me DHIS2 me response object
                           #' @param max_cache_age cache expiry currently used by datim validation
                           initialize = function(config_path = NA_character_,
                                                 base_url,
                                                 handle,
                                                 me) {
                             self$config_path <- config_path
                             self$me <- me
                             self$user_orgunit <- me$organisationUnits$id
                             self$base_url <- base_url
                             self$username <- me$userCredentials$username
                             self$handle <- handle
                           }
                         )
)

# Wed Sep  8 16:48:56 2021 ------------------
#' @title getOAuthToken (app_url,app,api,scope)
#'
#' @description retrieve authorization token from DATIM/DHIS2
#' @param app Oauthapp details
#' @return access token to specified application
#'
getOAuthToken <- function(redirect_uri,app,api,scope) {
  
  token <- oauth2.0_token(
    app = app,
    endpoint =api, 
    scope = scope,
    use_basic_auth = TRUE,
    oob_value=redirect_uri,
    cache = FALSE
  )
  
  return(token)
}

#REALLY NEED a better user input here that pops up on the screen as opposed to terminal or a way to extract that url

###############################################################################

#' @export
#' @title loginToDATIMfunction(config_path=NULL,
#' config_path_level = "dhis" )
#' @description logins into a datim or dhis2 api using either default keyring and 
#' a config file. This function creates a d2Session login object in the 
#' environment calling the login function.
#' E.g. global environment or R-shiny session. Thus you do not need to assign
#' the output of this function to a variable as it creates the variable/object
#' as a side effect.
#' @param config_path path to a dhis config file. If provided, username and password should not be provided.
#' @param config_path_level if there a multiple json entries in the config
#' file, it will default to dhis
#' @param username DHIS 2 username. If provided must provide password and config_path must be NULL
#' @param password DHIS 2 password for the username. If provided must provide password and config_path must be NULL
#' @param base_url if providing password and username directly this must be non null
#' @param d2_session_name the variable name for the d2Session object. The default
#' name is d2_default_session and will be used by other datimutils functions by default when 
#' connecting to datim. Generally a custom name should only be needed if you need to log into
#' two seperate DHIS2 instances at the same time. If you create a d2Session object with a
#' custom name then this object must be passed to other datimutils functions explicitly
#' @param d2_session_envir the environment in which to place the R6 login object, default
#' is the immediate calling environment
loginToDATIMOAuth <- function(config_path = NULL,
                         config_path_level = "dhis",
                         #username = NULL,
                         #password = NULL,
                         base_url = NULL,
                         token = NULL, #Added for testing token integration
                         redirect_uri= NULL,
                         app= NULL,
                         api= NULL,
                         scope= NULL,
                         d2_session_name = "d2_default_session",
                         d2_session_envir = parent.frame()) {
  
  # Thu Oct  7 16:53:33 2021 ------------------------------
  #Replaced 
  #get token
  #token=getOAuthToken(redirect_uri,app,api,scope)
  #With the below code block in order to be able to use it with shiny. Essentially
  # if the token is null it will take you to login at DHIS2 if you already have your token from 
  #Shiny it will use that. Thoughts?
  
  if (is.null(token)) {
    token=getOAuthToken(redirect_uri,app,api,scope)
  } else {
    token=token #For Shiny 
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
           d2Session$new(config_path = config_path,
                         base_url = base_url,
                         handle = handle,
                         me = me), 
           envir = d2_session_envir)
  }
  

}



#loginToDATIMOAuth(base_url = "play.dhis2.org/2.36.3/",app=app, api = api, redirect_uri=redirect_uri,scope = scope)

