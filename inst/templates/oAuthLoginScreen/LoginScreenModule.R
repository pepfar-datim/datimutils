### Shiny Module fo OAuth login

### Variables based upon the DHIS2 OAuth2 Client being integrated

if (interactive()) {
  # testing url
  options(shiny.port = 8100)
  APP_URL <- "http://127.0.0.1:8100/"
} else {
  # deployed URL
  APP_URL <- "c" #This will be your shiny server path
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

authenticated=FALSE #SOLELY FOR TESTING


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
      
      
### Login Button Checks
      observeEvent(input$login_button, {
        tryCatch({
          datimutils::loginToDATIM(base_url = Sys.getenv("BASE_URL"),
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
        flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
        ready$ok  <-  FALSE
        user_input$authenticated  <-  FALSE
        user_input$user_name <- ""
        user_input$authorized  <-  FALSE
        user_input$d2_session  <-  NULL
        d2_default_session <- NULL
        gc()
        session$reload()
        
      })
      
### OAUTH code

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

      #return(token) Alternate approach if loginToDATIMOAuth isn't useful
      
      loginToDATIMOAuth(base_url = "play.dhis2.org/2.36.4/",
                        token = token,
                        app=app,
                        api = api,
                        redirect_uri= APP_URL,
                        scope = scope)
      
      #I am not the biggest fan of using these global assigments, so completely open to suggestions. 
      d2_default_session<<-d2_default_session
      return(d2_default_session)
      
    })
  }


### UI Section
shinyOAuthUI <- function(id,uiBase) {
  ns <- NS(id)
  
  uiFunc <- function(req) {
    
    if ((authenticated=TRUE) & !has_auth_code(parseQueryString(req$QUERY_STRING))) { #IF does NOT have token take them to get one
      
      ### OAUTH ITEMS
      url <- oauth2.0_authorize_url(api, app, scope = scope)
      redirect <- sprintf("location.replace(\"%s\");", url)
      tags$script(HTML(redirect))
      
      ### Standard login screen 
      # wellPanel(fluidRow(
      #   img(src = "pepfar.png", align = "center"),
      #   h4("Welcome to the DataPack Validation App. Please login with your DATIM credentials:")
      # ),
      # fluidRow(
      #   textInput("user_name", "Username: ", width = "600px"),
      #   passwordInput("password", "Password:", width = "600px"),
      #   actionButton("login_button", "Log in!")
      # ),
      # fluidRow(
      #   tags$hr(),
      #   tags$div(HTML("<ul><li><h4>Please be sure you fully populate the PSNUxIM tab when receiving a new DataPack.",
      #                 "Consult <a href = \"https://apps.datim.org/datapack-userguide/\" target = \"blank\" > the user guide</a>",
      #                 "for further information!</h4></li><li><h4>See the latest updates to the app <a href =",
      #                 "\"https://github.com/pepfar-datim/datapackr-app/blob/master/CHANGELOG.md\"",
      #                 "target  = \"blank\">here.</h4></a></li></ul>"))
      # ),
      # tags$hr(),
      # #fluidRow(HTML(getVersionInfo()))
      # )
      # 
      
    } else {
      uiBase
    }
  }
  
}
