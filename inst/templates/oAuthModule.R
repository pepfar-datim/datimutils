library(shiny)
library(dplyr)
library(ggplot2)



# ### Shiny Module fo OAuth login
# 
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
        oob_value="http://127.0.0.1:8100/",#APP_URL, #"http://127.0.0.1:8100/", 
        cache = FALSE,
        credentials = oauth2.0_access_token(endpoint = api,
                                            app = app,
                                            code = params$code,
                                            use_basic_auth = TRUE)
      )

      #return(token) Alternate approach if loginToDATIMOAuth isn't useful
      
      loginToDATIMOAuth(base_url = "play.dhis2.org/2.36.4/",
                        token = token,
                        app=app,
                        api = api,
                        redirect_uri="http://127.0.0.1:8100/",
                        scope = scope)
      
      #I am not the biggest fan of using these global assigments, so completely open to suggestions. 
      d2_default_session<<-d2_default_session
      return(d2_default_session)
      

    })
  }


### UI Section
shinyOAuthUI <- function(id,uiBase,app,api,scope) {
  ns <- NS(id)
  
  uiFunc <- function(req) {
    if (!has_auth_code(parseQueryString(req$QUERY_STRING))) { #IF does NOT have token take them to
      url <- oauth2.0_authorize_url(api, app, scope = scope)
      redirect <- sprintf("location.replace(\"%s\");", url)
      tags$script(HTML(redirect))
    } else {
      uiBase
    }
  }
  
}
