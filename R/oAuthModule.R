### Shiny Module fo OAuth login

# params is a list object containing the parsed URL parameters. Return TRUE if
# based on these parameters, it looks like auth codes are present that we can
# use to get an access token. If not, it means we need to go through the OAuth
# flow.
has_auth_code <- function(params) {
  
  return(!is.null(params$code))
  
}

### UI Section
shinyOAuthUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(12,
             uiOutput(ns("oAuth_UI"))
             )
    )
  )
}

### Server Section
shinyOAuthServer <- function(id) { 
  
  moduleServer(
    id,
    function(input, output, session){
    
    output$oAuth_UI <- renderUI({
      
      ns <- session$ns
      
      #### Actual UI CODE #####
      uioAuth_UI  <- function(req) {
        if (!has_auth_code(parseQueryString(req$QUERY_STRING))) { #IF does NOT have token take them to
          url <- oauth2.0_authorize_url(api, app, scope = scope)
          redirect <- sprintf("location.replace(\"%s\");", url)
          tags$script(HTML(redirect))
        } else {
          ui
        }
      }
      
      
      
      
      ######################################################################
      params <- parseQueryString(isolate(session$clientData$url_search))
      observe({print(session$clientData$url_search)})
      
      if (!has_auth_code(params)) {
        return()
      }
      
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
      

    })
    
  })
}