library(shiny)
library(tidyverse)
library(httr)
library(xml2)
library(datimutils)
#source("~/Documents/Repos/datimutils/inst/templates/oAuthDirect/OAuthModule.R")
#Note the below will not be necessary once merged with datimutils
#source("~/Documents/Repos/datimutils/R/oAuthLogin.R")
{
    ### Define Functions
    d2Session <- R6::R6Class("d2Session",
                             #' @title d2Session 
                             public=list(
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
    
    loginToDATIMOAuth <- function(
        base_url = NULL,
        token = NULL, 
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
        # if the token is null it will take you to login at DHIS2 if you already have your token(From Shiny)
        #it will use that. Thoughts @Sam?
        
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
                   d2Session$new(base_url = base_url,
                                 handle = handle,
                                 me = me), 
                   envir = d2_session_envir)
        }
        
        
    }
}


### Variables based upon the DHIS2 OAuth2 Client being integrated
options(httr_oob_default=TRUE)

if (interactive()) {
    # testing url
    options(shiny.port = 8100)
    APP_URL <- "http://127.0.0.1:8100/"
} else {
    # deployed URL
    APP_URL <- "https://rstudio-connect.testing.ap.datim.org/content/96" #This will be your shiny server path
}

app <- oauth_app("OAuth2 Demo Client", #dhis2 = Name
                 key = "demo",         #dhis2 = Client ID
                 secret = "31712fa82-b9ae-8df6-750b-0ce9ca0f9b1", #dhis2 = Client Secret
                 redirect_uri = APP_URL #"http://127.0.0.1:8100/"
)

api <- oauth_endpoint(base_url = "https://play.dhis2.org/2.36.4/uaa/oauth",
                      request=NULL,#Documentation says to leave this NULL for OAuth2 
                      authorize = "authorize",
                      access="token"
) 

scope <- "ALL"

has_auth_code <- function(params) {
    
    return(!is.null(params$code))
    
}

#uiBase 
ui <- fluidPage(
    titlePanel("Hello Shiny!"),
    verbatimTextOutput("code"),
    DT::dataTableOutput("mytable")
)

uiFunc <- function(req) {
    if (!has_auth_code(parseQueryString(req$QUERY_STRING))) { #IF does NOT have token take them to
        url <- oauth2.0_authorize_url(api, app, scope = scope)
        redirect <- sprintf("location.replace(\"%s\");", url)
        tags$script(HTML(redirect))
    } else {
        ui
    }
}
#ui <- shinyOAuthUI("oAuth",uiBase)

server <- function(input, output, session) {
    
    #shinyOAuthServer("oAuth")
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
                      scope = scope,
                      d2_session_envir = parent.env(environment()) #Based on Sam's advice, def want this in there. 
    )
    
        
    df=getMetadata(
        end_point = "organisationUnits",
        "organisationUnitGroups.name:eq:District",
        fields = "id,name,level"
    )

    output$mytable = DT::renderDataTable({
        df
    })
    
    base_url="play.dhis2.org/2.36.4/"
    url <- utils::URLencode(URL = paste0(base_url, "api", "/me"))
    handle <- httr::handle(base_url)
    
    #curl command to get the above 
    resp <- GET(url, config(token = token))
    output$code <- renderText(content(resp, "text"))
    
}

shinyApp(ui = uiFunc, server = server)