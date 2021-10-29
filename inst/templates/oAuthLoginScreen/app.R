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
                     secret = "3f6a26d22-6ee0-2886-46d5-90a6c6f4d40", #dhis2 = Client Secret
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
    
    observeEvent(input$back_to_login,{
        req(input$back_to_login)
        updateQueryString("?",mode="replace",session=session)
        session$reload()
    })
    
    output$ui_hasauth = renderUI({
        req(input$login_button)
        print("login_button")
        req(input$user_name)
        req(input$password)
        if(input$user_name=="admin" & input$password =="jbales"){
            print("auth OK")
            hashcode = safer::encrypt_string(paste0(input$user_name,input$password,'@time:',Sys.time()),key = mykey)
            redirect <- sprintf("location.replace(\"%s\");", paste0(APP_URL,"?code=",hashcode))
            tags$script(HTML(redirect))
            
        }
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
    
    output$code <- renderText({
        
        params <- parseQueryString(session$clientData$url_search)
        print(params)
        req(has_auth_code(params))
        
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
        
        
#################### Lets us know about the request ############################
        print(paste0("status code : ",resp$status_code))
        
        if(resp$status_code == 200){
            res = content(resp, "text")
            
        } else {
            
            #check the token was produced less that 1 minute ago (maybe reduce this time ?)
            has_auth_user_pwd = safer::decrypt_string(params$code,key = mykey) %>% 
                strsplit(split = "@time:") %>% 
                .[[1]] %>% 
                .[2] %>%
                as.POSIXct()
            
            has_auth_user_pwd = difftime(Sys.time(),has_auth_user_pwd,units = "mins")<1
            if (has_auth_user_pwd){
                res = "Auth with user & password"
            } else {
                res = "Bad auth"
            }
        }
        res
    })
}


############### UI #############################################################

### Developers will place their AFTER AUTHENTICATED code in this ui block 
ui <- function(req_txt) {
    fluidPage(
        titlePanel("Hello Shiny!"),
        verbatimTextOutput("code"),
        DT::dataTableOutput("mytable"),
        #textAreaInput("inreq",NULL,value = req_txt,height = "300px"),
        actionButton("back_to_login","Return to Login Page",icon=icon("home"))
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
    
    if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
        ui_auth(req_txt)
    } else {
        ui(req_txt)
    }
}

# NOTE: uiFunc, as opposed to ui
shinyApp(uiFunc, server)