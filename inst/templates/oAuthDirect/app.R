library(shiny)
library(tidyverse)
library(httr)
library(xml2)
library(datimutils)

# Note will not be necessary once merged with datimutils
source("~/Documents/Repos/datimutils/R/oAuthLogin.R")

### Variables based upon the DHIS2 OAuth2 Client being integrated
options(httr_oob_default=TRUE)

if (interactive()) {
    # testing url
    options(shiny.port = 8100)
    APP_URL <- "http://127.0.0.1:8100/"# This will be your local host path
} else {
    # deployed URL
    APP_URL <- "https://rstudio-connect.testing.ap.datim.org/content/96"# This will be your shiny server path
}

app <- oauth_app("OAuth2 Demo Client", #dhis2 = Name
                 key = "demo",         #dhis2 = Client ID
                 secret = "852fa0557-1e3e-aec6-6e3d-b8891223c73",# dhis2 = Client Secret
                 redirect_uri = APP_URL 
)

api <- oauth_endpoint(base_url = "https://play.dhis2.org/2.36.7/uaa/oauth",
                      request=NULL,# Documentation says to leave this NULL for OAuth2 
                      authorize = "authorize",
                      access="token"
) 

scope <- "ALL"

has_auth_code <- function(params) {
    
    return(!is.null(params$code))
    
}

ui <- fluidPage(
    titlePanel("Hello Shiny!"),
    verbatimTextOutput("code"),
    DT::dataTableOutput("mytable")
)

uiFunc <- function(req) {
    if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
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
    
    # Manually create a token
    token <- oauth2.0_token(
        app = app,
        endpoint =api, 
        scope = scope,
        use_basic_auth = TRUE,
        oob_value=APP_URL,
        cache = FALSE,
        credentials = oauth2.0_access_token(endpoint = api,
                                            app = app,
                                            code = params$code,
                                            use_basic_auth = TRUE)
    )
    
    loginToDATIMOAuth(base_url = "play.dhis2.org/2.36.7/",
                      token = token,
                      app=app,
                      api = api,
                      redirect_uri= APP_URL,
                      scope = scope,
                      d2_session_envir = parent.env(environment())
    )
    
        
    df=getMetadata(
        end_point = "organisationUnits",
        "organisationUnitGroups.name:eq:District",
        fields = "id,name,level"
    )

    output$mytable = DT::renderDataTable({
        df
    })
    
    base_url="play.dhis2.org/2.36.7/"
    url <- utils::URLencode(URL = paste0(base_url, "api", "/me"))
    handle <- httr::handle(base_url)
    
    #curl command to get the above 
    resp <- GET(url, config(token = token))
    output$code <- renderText(content(resp, "text"))
    
}

shinyApp(ui = uiFunc, server = server)