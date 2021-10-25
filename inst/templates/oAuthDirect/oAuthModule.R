### Shiny Module fo OAuth login

### Variables based upon the DHIS2 OAuth2 Client being integrated

if (interactive()) {
  # testing url
  options(shiny.port = 8100)
  APP_URL <- "http://127.0.0.1:8100/"
} else {
  # deployed URL
  APP_URL <- "https://rstudio-connect.testing.ap.datim.org/content/92" #This will be your shiny server path
}

app <- oauth_app("OAuth2 Demo Client", #dhis2 = Name
                 key = "demo",         #dhis2 = Client ID
                 secret = "cc0d5b9e4-8450-31fb-e9a6-50d2efb0b2e", #dhis2 = Client Secret
                 redirect_uri = APP_URL #"http://127.0.0.1:8100/"
)

api <- oauth_endpoint(base_url = "https://play.dhis2.org/2.36.4/uaa/oauth",
                      request=NULL,#Documentation says to leave this NULL for OAuth2 
                      authorize = "authorize",
                      access="token"
) 

scope <- "ALL"

# # params is a list object containing the parsed URL parameters. Return TRUE if
# # based on these parameters, it looks like auth codes are present that we can
# # use to get an access token. If not, it means we need to go through the OAuth
# # flow.
has_auth_code <- function(params) {
  
  return(!is.null(params$code))
  
}

# ### Server Section
shinyOAuthServer <- function(id) {
  
  moduleServer(id,function(input, output, session){
    
    ns <- session$ns
    
    params <- parseQueryString(isolate(session$clientData$url_search))
    if (!has_auth_code(params)) {
      return()
    }
    
    # Link to Documenation https://httr.r-lib.org/reference/oauth2.0_token.html
    #options(httr_oob_default=TRUE) This might have to be ran in the users console the first time.
    
    # Manually create a token
    token <- oauth2.0_token(
      app = app,
      endpoint =api, 
      scope = scope,
      use_basic_auth = TRUE,
      oob_value=APP_URL, #"http://127.0.0.1:8100/", 
      cache = FALSE,
      credentials = oauth2.0_access_token(endpoint = api,
                                          app = app,
                                          code = params$code,
                                          use_basic_auth = TRUE)
    )
    
    loginToDATIMOAuth(base_url = "play.dhis2.org/2.36.4/",
                      token = token,
                      app=app,
                      api = api,
                      redirect_uri= APP_URL,
                      scope = scope)
    

    d2_default_session<<-d2_default_session #This probably needs refactored, becasue of the use of <<-. Removing this line does 
                                              #in fact still put the d2 session obeject in the users environment, but only after
                                                # closing the app launch. Perhaps add a trigger or if else to the 'after' code
                                                  #Or in a more shiny approach observeEvent. Sam mentioned a fix via a slack thread 
                                                    #that deals with the shiny environemnt scoping
    return(d2_default_session)
    
  })
}


### UI Section
shinyOAuthUI <- function(id,uiBase) {
  ns <- NS(id)
  
  uiFunc <- function(req) {
    
    if (!has_auth_code(parseQueryString(req$QUERY_STRING))) { #IF user does NOT have token take them to get one
      
      ### OAUTH ITEMS
      url <- oauth2.0_authorize_url(api, app, scope = scope)
      redirect <- sprintf("location.replace(\"%s\");", url)
      tags$script(HTML(redirect))
      
      
    } else {
      uiBase
    }
  }
  
}
