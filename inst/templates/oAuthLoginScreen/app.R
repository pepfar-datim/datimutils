################ Libraries, Functions, Variables ###############################
library(shiny)
library(httr)
library(shinydashboard)
library(magrittr)
library(tidyverse)
library(xml2)
library(datimutils)
library(waiter)
library(futile.logger)
library(shinyWidgets)

#NOTE this will go away
#source("~/Documents/Repos/datimutils/R/oAuthLogin.R")

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

# Wed Sep  8 16:48:56 2021 ------------------
#' @title getOAuthToken (app_url,app,api,scope)
#' @description retrieve authorization token from DATIM/DHIS2
#' @param app OAuth2 client details - Name, Client ID, Client Secret, redirect URIs
#' @param redirect_uri Redirect_uri listed under client details
#' @param api Endpoint for authorization - base url, authorize endpoint, access token endpoint
#' @param scope the scope of what the client will return. "All" is default for DHIS2
#' @return access token to specified OAuth2 Client
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
#' @param app OAuth2 client details - Name, Client ID, Client Secret, redirect URIs
#' @param api Endpoint for authorization - base url, authorize endpoint, access token endpoint
#' @param scope The scope of what the client will return. "All" is default for DHIS2
#' @param d2_session_name the variable name for the d2Session object. The default
#' name is d2_default_session and will be used by other datimutils functions by default when 
#' connecting to datim. Generally a custom name should only be needed if you need to log into
#' two seperate DHIS2 instances at the same time. If you create a d2Session object with a
#' custom name then this object must be passed to other datimutils functions explicitly
#' @param d2_session_envir the environment in which to place the R6 login object, default
#' is the immediate calling environment
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








######## APP starts
if (interactive()) {
    # testing url
    options(shiny.port = 8100)
    APP_URL <- "http://127.0.0.1:8100/" #Investigate this further if time permits
} else {
    # deployed URL
    APP_URL <- "https://rstudio-connect.testing.ap.datim.org/content/97"
}

################ OAuth Client information ##################################### 
{
    app <- oauth_app("OAuth2 Demo Client", #dhis2 = Name
                     key = "demo",         #dhis2 = Client ID
                     secret = "f036759e2-ab67-d40b-b46f-722a1503c05", #dhis2 = Client Secret
                     redirect_uri = APP_URL #"http://127.0.0.1:8100/"
    )
    
    api <- oauth_endpoint(base_url = "https://play.dhis2.org/2.36.4/uaa/oauth",
                          request=NULL,#Documentation says to leave this NULL for OAuth2 
                          authorize = "authorize",
                          access="token"
    ) 
    
    scope <- "ALL"
}

has_auth_code <- function(params) {
    
    return(!is.null(params$code))
}

mykey = paste(sample(LETTERS,20,replace = T),collapse="") #Just an added layer of security, feel free to remove here and the output$ui_hasauth block
############### SERVER #########################################################
server <- function(input, output, session) {
    
    #Username and PW login Option
    output$ui_hasauth = renderUI({
        req(input$login_button)
        req(input$user_name)
        req(input$password)
            
         hashcode = safer::encrypt_string(paste0(input$user_name,input$password,'@time:',Sys.time()),key = mykey)

         if(user_input$authenticated  ==  TRUE){
             
            hashcode = safer::encrypt_string(paste0(input$user_name,input$password,'@time:',Sys.time()),key = mykey)
            
             redirect <- sprintf("location.replace(\"%s\");", paste0(APP_URL,"?code="))
             tags$script(HTML(redirect))
            
       } 
    })
    
    ### Login Button Checks 
    observeEvent(input$login_button, {    
        tryCatch({
            datimutils::loginToDATIM(base_url = "https://play.dhis2.org/2.36.4/", #Sys.getenv("BASE_URL"),Modified for testing
                                     username = input$user_name,
                                     password = input$password,
                                     d2_session_envir = parent.env(environment())
            )
        },
        # This function throws an error if the login is not successful
        error = function(e) {
            flog.info(paste0("User ", input$user_name, " login failed."), name = "datapack")
        }
        )
        if (exists("d2_default_session")) {
            if (any(class(d2_default_session) == "d2Session")) {
                user_input$authenticated  <-  TRUE
                user_input$d2_session  <-  d2_default_session$clone()
                d2_default_session <- NULL
                
                # Need to check the user is a member of the PRIME Data Systems Group, COP Memo group, or a super user
                user_input$memo_authorized  <-
                    grepl("VDEqY8YeCEk|ezh8nmc4JbX", user_input$d2_session$me$userGroups) |
                    grepl(
                        "jtzbVV4ZmdP",
                        user_input$d2_session$me$userCredentials$userRoles
                    )
                flog.info(
                    paste0(
                        "User ",
                        user_input$d2_session$me$userCredentials$username,
                        " logged in."
                    ),
                    name = "datapack"
                )
            }
        } else {
            sendSweetAlert(
                session,
                title = "Login failed",
                text = "Please check your username/password!",
                type = "error"
            )
        }
    })  
    
    ### Logout Button Checks
    observeEvent(input$logout, {
        req(input$logout)
        updateQueryString("?",mode="replace",session=session) #Gets you back to the login without the authorization code at top
        flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
        user_input$authenticated  <-  FALSE
        user_input$user_name <- ""
        user_input$authorized  <-  FALSE
        user_input$d2_session  <-  NULL
        d2_default_session <- NULL
        gc()
        session$reload()
    })
    
    output$ui_redirect = renderUI({
        #print(input$login_button_oauth) useful for debugging 
        if(!is.null(input$login_button_oauth)){
            if(input$login_button_oauth>0){
                url <- oauth2.0_authorize_url(api, app, scope = scope)
                redirect <- sprintf("location.replace(\"%s\");", url)
                tags$script(HTML(redirect))
            } else NULL
        } else NULL
    })
    
    ### Login Button oauth Checks 
    observeEvent(input$login_button_oauth > 0,{
        
        #Grabs the code from the url
        params <- parseQueryString(session$clientData$url_search)
        req(has_auth_code(params)) 
        
        tryCatch({
            
        #Manually create a token
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
            #This could also be developed further in order to get a token with 
                #username & pw, but defeats the point.
            # user_params = list(grant_type = "password",
            #                    username=input$username,
            #                    password=input$password),
            # use_oob=T
            
        )
        
        loginToDATIMOAuth(base_url = "play.dhis2.org/2.36.4/",
                          token = token,
                          app=app,
                          api = api,
                          redirect_uri= APP_URL,
                          scope = scope,
                          d2_session_envir = parent.env(environment()) #Based on Sam's advice, def want this in there.
        )
    },
    error = function(e) {
        flog.info(paste0("User ", input$user_name, " login failed."), name = "datapack")
    }
        )
        
        # if(has_auth_code(params)){
        if (exists("d2_default_session")) {
            if (any(class(d2_default_session) == "d2Session")) {
                user_input$authenticated  <-  TRUE
                user_input$d2_session  <-  d2_default_session$clone()
                d2_default_session <- NULL
                
                # Need to check the user is a member of the PRIME Data Systems Group, COP Memo group, or a super user
                user_input$memo_authorized  <-
                    grepl("VDEqY8YeCEk|ezh8nmc4JbX", user_input$d2_session$me$userGroups) |
                    grepl(
                        "jtzbVV4ZmdP",
                        user_input$d2_session$me$userCredentials$userRoles
                    )
                flog.info(
                    paste0(
                        "User ",
                        user_input$d2_session$me$userCredentials$username,
                        " logged in."
                    ),
                    name = "datapack"
                )
            }
        } else {

        }

    })
    
    ### Controls Table   
    output$mytable = DT::renderDataTable({
        
        df=getMetadata(
            end_point = "organisationUnits",
            "organisationUnitGroups.name:eq:District",
            fields = "id,name,level",
            d2_session = user_input$d2_session
        )
        
    })
    
    #Check if the user is authenticated via console
    observe(print(isolate(user_input$authenticated)))
}
############### UI #############################################################

### Developers will place their AFTER AUTHENTICATED code in this ui block 
ui <- function(req_txt) {
    fluidPage(
        titlePanel("OAuth or PW Login Template"),
        DT::dataTableOutput("mytable"),
        actionButton("logout",
                     "Return to Login Page",
                     icon=icon("sign-out"))
    )
}

### Login Screen
ui_auth <- function(req_txt){
    fluidPage(
        fluidRow(
            column(width = 2,
                   offset = 5,
                   br(),
                   br(),
                   br(),
                   br(),
        wellPanel(
              fluidRow(
                  tags$div(HTML('<center><img src="pepfar.png"></center>')),
                  h4("Welcome to the DataPack Validation App. Please login with your DATIM credentials:")
              ),
              fluidRow(
                  textInput("user_name","Username",width = "600px"),
                  passwordInput("password","Password",width = "600px"),
                  actionButton("login_button","Log in!"),
                  actionButton("login_button_oauth","Log in with DATIM"),
                  uiOutput("ui_hasauth"),
                  uiOutput("ui_redirect")
              ),
              fluidRow(
                  tags$hr(),
                  tags$div(HTML('<ul><li><h4>Please be sure you fully populate the PSNUxIM tab when receiving a new DataPack. 
           Consult <a href="https://apps.datim.org/datapack-userguide/" target = "_blank" > the user guide</a> for further information!</h4></li>
                    <li><h4>See the latest updates to the app <a href="https://github.com/pepfar-datim/datapackr-app/blob/master/CHANGELOG.md" target ="_blank">here.</h4></a></li></ul>'))
              ),
              tags$hr()#,
              # fluidRow(HTML(getVersionInfo()))
              
        )
    )))
}

#Assists with stand UserName and PW method
user_input <- reactiveValues(authenticated = FALSE,
                             status = "",
                             d2_session = NULL)

### Checks and controls the two ui options
uiFunc <- function(req) {
    # req_txt = paste0("REQUEST_METHOD: ",req$REQUEST_METHOD,"\n",
    #                  "SCRIPT_NAME: ", req$SCRIPT_NAME,"\n",
    #                  "PATH_INFO: ", req$PATH_INFO,"\n",
    #                  "QUERY_STRING: ",req$QUERY_STRING,"\n",
    #                  "SERVER_NAME: ",req$SERVER_NAME,"\n",
    #                  "SERVER_PORT: ",req$SERVER_PORT,"\n",
    #                  "HTTP_CONNECTION: ",req$HTTP_CONNECTION,"\n",
    #                  "HTTP_UPGRADE_INSECURE_REQUESTS: ",req$HTTP_UPGRADE_INSECURE_REQUESTS,"\n",
    #                  "HTTP_ACCEPT: ",req$HTTP_ACCEPT,"\n",
    #                  "HTTP_ACCEPT_LANGUAGE: ",req$HTTP_ACCEPT_LANGUAGE,"\n",
    #                  "HTTP_ACCEPT_ENCODING: ",req$HTTP_ACCEPT_ENCODING,"\n",
    #                  "HTTP_USER_AGENT: ",req$HTTP_USER_AGENT,"\n",
    #                  "HTTP_HOST: ",req$HTTP_HOST,"\n"
    # )
    #print(names(req)) #Useful for debugging, will print above info to console
    
     if (!has_auth_code(parseQueryString(req$QUERY_STRING))){ # ( isolate(user_input$authenticated)  ==  FALSE )
         ui_auth(req_txt)
    } else {
         ui(req_txt)
     }
}

# NOTE: uiFunc, as opposed to ui
shinyApp(uiFunc, server)