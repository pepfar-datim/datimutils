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

source("~/Documents/Repos/datimutils/R/oAuthLogin.R")


if (interactive()) {
    # testing url
    options(shiny.port = 8100)
    APP_URL <- "http://127.0.0.1:8100/" #Investigate this further if time permits
} else {
    # deployed URL
    APP_URL <- "https://servername/path-to-app"
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

mykey = paste(sample(LETTERS,20,replace = T),collapse="")#Will remove after dev
############### SERVER #########################################################
server <- function(input, output, session) {
    

    #Username and PW login Option
    output$ui_hasauth = renderUI({
        req(input$login_button)
        req(input$user_name)
        req(input$password)
            
         hashcode = safer::encrypt_string(paste0(input$user_name,input$password,'@time:',Sys.time()),key = mykey)

         if(user_input$authenticated  ==  TRUE){
            #print("auth OK")
            hashcode = safer::encrypt_string(paste0(input$user_name,input$password,'@time:',Sys.time()),key = mykey)
            
             redirect <- sprintf("location.replace(\"%s\");", paste0(APP_URL,"?code="))
             tags$script(HTML(redirect))
            
       } 
    })
    
    ##Logic ported Datapackr-app
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
        #ready$ok  <-  FALSE #Ask Jason what this does
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
  


#     output$code <- renderText({
# 
#         # params <- parseQueryString(session$clientData$url_search)
#         # #print(params)
#         # req(has_auth_code(params))
#         # 
#         # if(has_auth_code(params)){
#         #     user_input$authenticated  <-  TRUE 
#         # }
#         # 
#         # #Manually create a token
#         # token <- oauth2.0_token(
#         #     app = app,
#         #     endpoint =api,
#         #     scope = scope,
#         #     use_basic_auth = TRUE,
#         #     oob_value=APP_URL, #"http://127.0.0.1:8100/",
#         #     cache = FALSE,
#         #     credentials = oauth2.0_access_token(endpoint = api,
#         #                                         app = app,
#         #                                         code = params$code,
#         #                                         use_basic_auth = TRUE)
#         #     #Just so we know this will also work
#         #     # user_params = list(grant_type = "password",
#         #     #                    username="admin",
#         #     #                    password="district"),
#         #     # use_oob=T
#         # 
#         # )
#         # 
#         # 
#         # loginToDATIMOAuth(base_url = "play.dhis2.org/2.36.4/",
#         #                   token = token,
#         #                   app=app,
#         #                   api = api,
#         #                   redirect_uri= APP_URL,
#         #                   scope = scope,
#         #                   d2_session_envir = parent.env(environment()) #Based on Sam's advice, def want this in there.
#         # )
#          
# 
#         
#         base_url="play.dhis2.org/2.36.4/"
#         url <- utils::URLencode(URL = paste0(base_url, "api", "/me"))
#         handle <- httr::handle(base_url)
#         
#         #curl command to get the above 
#         resp <- GET(url, config(token = token))
#         
#         
# #################### Lets us know about the request ############################
#         print(paste0("status code : ",resp$status_code))
#         
#         if(resp$status_code == 200){
#             res = content(resp, "text")
#             
#         } else {
#             
#             #check the token was produced less that 1 minute ago (maybe reduce this time ?)
#             has_auth_user_pwd = safer::decrypt_string(params$code,key = mykey) %>% 
#                 strsplit(split = "@time:") %>% 
#                 .[[1]] %>% 
#                 .[2] %>%
#                 as.POSIXct()
#             
#             has_auth_user_pwd = difftime(Sys.time(),has_auth_user_pwd,units = "mins")<1
#             if (has_auth_user_pwd){
#                 res = "Auth with user & password"
#             } else { 
#                 res = "Bad auth"
#             }
#         }
#         res
#     })
    
    #Controls table output
    output$mytable = DT::renderDataTable({
        
        params <- parseQueryString(session$clientData$url_search)
        #print(params)
        req(has_auth_code(params))
        
        if(has_auth_code(params)){
            user_input$authenticated  <-  TRUE 
        }
        
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
            #Just so we know this will also work
            # user_params = list(grant_type = "password",
            #                    username="admin",
            #                    password="district"),
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
        

        df=getMetadata(
            end_point = "organisationUnits",
            "organisationUnitGroups.name:eq:District",
            fields = "id,name,level"#,
            #d2_session = user_input$d2_session
        )
        
    })
    

    #Check if the user is authenticated via console
    observe(print(isolate(user_input$authenticated)))
}
############### UI #############################################################

### Developers will place their AFTER AUTHENTICATED code in this ui block 
ui <- function(req_txt) {
    fluidPage(
        titlePanel("Hello Shiny!"),
        #verbatimTextOutput("code"),
        DT::dataTableOutput("mytable"),
        #textAreaInput("inreq",NULL,value = req_txt,height = "300px"),
        actionButton("logout","Return to Login Page",icon=icon("sign-out"))
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
              
              #textAreaInput("inreq",NULL,value = req_txt,height = "300px")
        )
    )))
}

user_input <- reactiveValues(authenticated = FALSE,
                             status = "",
                             d2_session = NULL)

# test <- reactive( if ( (isolate(user_input$authenticated)  ==  FALSE) || ( has_auth_code(parseQueryString(req$QUERY_STRING)) )    ) T 
#                   else F )

### Checks and controls the two ui options
uiFunc <- function(req) {
    req_txt = paste0("REQUEST_METHOD: ",req$REQUEST_METHOD,"\n",
                     "SCRIPT_NAME: ", req$SCRIPT_NAME,"\n",
                     "PATH_INFO: ", req$PATH_INFO,"\n",
                     "QUERY_STRING: ",req$QUERY_STRING,"\n",
                     "SERVER_NAME: ",req$SERVER_NAME,"\n",
                     "SERVER_PORT: ",req$SERVER_PORT,"\n",
                     "HTTP_CONNECTION: ",req$HTTP_CONNECTION,"\n",
                     "HTTP_UPGRADE_INSECURE_REQUESTS: ",req$HTTP_UPGRADE_INSECURE_REQUESTS,"\n",
                     "HTTP_ACCEPT: ",req$HTTP_ACCEPT,"\n",
                     "HTTP_ACCEPT_LANGUAGE: ",req$HTTP_ACCEPT_LANGUAGE,"\n",
                     "HTTP_ACCEPT_ENCODING: ",req$HTTP_ACCEPT_ENCODING,"\n",
                     "HTTP_USER_AGENT: ",req$HTTP_USER_AGENT,"\n",
                     "HTTP_HOST: ",req$HTTP_HOST,"\n"
    )
    #print(names(req)) #Useful for debugging, will print above info to console
    
     if ( ( !has_auth_code(parseQueryString(req$QUERY_STRING))) ) { #If the user does NOT have a token OR hasn't logged in with a username and password
         ui_auth(req_txt)
    } else {
         ui(req_txt)
     }
    

    # ( isolate(user_input$authenticated)  ==  FALSE )
    
}

# NOTE: uiFunc, as opposed to ui
shinyApp(uiFunc, server)