
############## LIVE ONE THAT WORKS ################



library(shiny)
library(httr)
library(xml2)

# OAuth setup --------------------------------------------------------

#Prep for local or deployed app

if (interactive()) {
  # testing url
  options(shiny.port = 8100)
  APP_URL <- "http://127.0.0.1:8100/"
} else {
  # deployed URL
  APP_URL <- "https://rstudio-connect.testing.ap.datim.org/OAuth2/" #This will be your shiny server path
}

#Documentation 
# Note that secret is not really secret, and it's fine to include inline
# Copy information directly from the dhis2.org web settings
# Copy information directly from the dhis2.org web settings
app <- oauth_app("OAuth2 Demo Client", #dhis2 = Name
                 key = "demo",         #dhis2 = Client ID
                 secret = "76fd0224e-923c-fc67-151a-7619de5c5f7", #dhis2 = Client Secret
                 redirect_uri = APP_URL#"http://127.0.0.1:8100/"
)


# Create endpoints with oauth_endpoint()
# Fri Sep 24 16:49:46 2021 ------ This is def where the problem arises
#Documentation link https://httr.r-lib.org/reference/oauth_endpoint.html 

api <- oauth_endpoint(base_url = "https://play.dhis2.org/2.36.4/uaa/oauth",
                      request=NULL,#Documentation says to leave this NULL for OAuth2 
                      authorize = "authorize",
                      access="token"
                      
                      
) 

#This is rather open and can be toned back as deemed necessary

scope <- "ALL"

# Shiny -------------------------------------------------------------------

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth codes are present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}

# Your regular UI goes here, for when everything is properly auth'd
ui <- fluidPage(
  verbatimTextOutput("code")
)

# A little-known feature of Shiny is that the UI can be a function, not just
# objects. You can use this to dynamically render the UI based on the request.
# We're going to pass this uiFunc, not ui, to shinyApp(). If you're using
# ui.R/server.R style files, that's fine too--just make this function the last
# expression in your ui.R file.
uiFunc <- function(req) {
  if (!has_auth_code(parseQueryString(req$QUERY_STRING))) { #IF does NOT have token take them to
    url <- oauth2.0_authorize_url(api, app, scope = scope)
    redirect <- sprintf("location.replace(\"%s\");", url)
    tags$script(HTML(redirect))
  } else {
    ui
  }
}

server <- function(input, output, session) {
  
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
  
  ##### Print out variables for tracing purposes ######
  print(params$code)
  print(app$redirect_uri)
  
  # form url
  base_url="play.dhis2.org/2.36.4/"
  url <- utils::URLencode(URL = paste0(base_url, "api", "/me"))
  handle <- httr::handle(base_url)
  
  #curl command to get the above 
  resp <- GET(url, config(token = token))
  # TODO: check for success/failure here
  #outputs curl command to 
  output$code <- renderText(content(resp, "text"))
  
}

# Note that we're using uiFunc, not ui!
shinyApp(uiFunc, server)