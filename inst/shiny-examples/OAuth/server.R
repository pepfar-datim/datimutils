### Libraries
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


### js ----
# allows for using the enter button for the log in
jscode_login <- '$(document).keyup(function(e) {
    var focusedElement = document.activeElement.id;
    console.log(focusedElement);
    if (e.key == "Enter" && focusedElement == "user_name") {
    $("#password").focus();
    } else if (e.key == "Enter" && focusedElement == "password") {
    $("#login_button").click();
    }
});'

### Initiate logging
logger <- flog.logger()
flog.appender(appender.console(), name = "datimutils")

### OAuth Client information
if (interactive()) {
    # NOTE: The line below must be ran manually to set the port
    # OR this line can be added to .Rprofile.
    # This is not an issue when using a single file version of shiny, ie app.R
    # The order by which the files execute is the reasoning behind this.
    options(shiny.port = 3123)
    # testing url
    APP_URL <- "http://127.0.0.1:3123/"# This will be your local host path
} else {
    # deployed URL
    APP_URL <- Sys.getenv("APP_URL") #This will be your shiny server path
}

 oauth_app <- httr::oauth_app(Sys.getenv("OAUTH_APPNAME"),
                                 key = Sys.getenv("OAUTH_KEYNAME"), # dhis2 = Client ID
                                 secret = Sys.getenv("OAUTH_SECRET"), #dhis2 = Client Secret
                                 redirect_uri = APP_URL
    )

    oauth_api <- httr::oauth_endpoint(base_url = paste0(Sys.getenv("BASE_URL"), "uaa/oauth"),
                                      request = NULL,
                                      authorize = "authorize",
                                      access = "token"
    )

    oauth_scope <- "ALL"


has_auth_code <- function(params) {

    return(!is.null(params$code))
}

###  Begin Shiny Web app items
shinyServer(function(input, output, session) {

    validation_results <- reactive({ validate() }) # nolint

    ready <- reactiveValues(ok = FALSE)

    user_input <- reactiveValues(authenticated = FALSE,
                                 status = "",
                                 d2_session = NULL,
                                 memo_authorized = FALSE,
                                 uuid = NULL)

###  Logout
    observeEvent(input$logout, {
        req(input$logout)
        # Returns to the log in screen without the authorization code at top
        updateQueryString("?", mode = "replace", session = session)
        flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
        ready$ok <- FALSE
        user_input$authenticated <- FALSE
        user_input$user_name <- ""
        user_input$authorized <- FALSE
        user_input$d2_session <- NULL
        d2_default_session <- NULL
        gc()
        session$reload()
    })

###  Logic for which UI to display
    output$ui <- renderUI({

        if (user_input$authenticated == FALSE) {
            # References the UI code for log in page, could be combined
            fluidPage(
                fluidRow(
                    column(width = 2, offset = 5,
                           br(), br(), br(), br(),
                           uiOutput("uiLogin")
                    )
                )
            )
        } else {
            #References the UI code for after log in, could be combined
            uiOutput("authenticated")
        }
    })

### UI code for log in screen
    # Username and password text fields, log in button
    output$uiLogin <- renderUI({

        wellPanel(fluidRow(
            tags$head(tags$script(HTML(jscode_login))), # enter button functionality for login button
            #tags$div(HTML('<center><img src="pepfar.png"></center>')), #Can add logo
            h4("Welcome to the Datimutils OAuth Example App. Please login with your DATIM credentials:")
        ),
        fluidRow(
            actionButton("login_button_oauth", "Log in with DATIM"),
            uiOutput("ui_hasauth"),
            uiOutput("ui_redirect")
        ),

        )
    })

### UI code for after log in
    output$authenticated <- renderUI({

        fluidPage(
            titlePanel("Datimutils OAuth Example App"),
            DT::dataTableOutput("mytable"),
            actionButton("logout",
                         "Return to Login Page",
                         icon = icon("sign-out"))
        )
    })

 #UI that will display when redirected to OAuth login agent
    output$ui_redirect <- renderUI({
        #print(input$login_button_oauth) useful for debugging
        if (!is.null(input$login_button_oauth)) { # nolint
            if (input$login_button_oauth > 0) { # nolint
                url <- httr::oauth2.0_authorize_url(oauth_api, oauth_app, scope = oauth_scope)
                redirect <- sprintf("location.replace(\"%s\");", url)
                tags$script(HTML(redirect))
            } else NULL
        } else NULL
    })

### Login Button oauth Checks
    observeEvent(input$login_button_oauth > 0, {

        #Grabs the code from the url
        params <- parseQueryString(session$clientData$url_search)
        #Wait until the auth code actually exists
        req(has_auth_code(params))

        #Manually create a token
        token <- httr::oauth2.0_token(
            app = oauth_app,
            endpoint = oauth_api,
            scope = oauth_scope,
            use_basic_auth = TRUE,
            oob_value = APP_URL,
            cache = FALSE,
            credentials = httr::oauth2.0_access_token(endpoint = oauth_api,
                                                      app = oauth_app,
                                                      code = params$code,
                                                      use_basic_auth = TRUE)
        )

        loginAttempt <- tryCatch({
            user_input$uuid <- uuid::UUIDgenerate()
            datimutils::loginToDATIMOAuth(base_url =  Sys.getenv("BASE_URL"),
                                          token = token,
                                          app = oauth_app,
                                          api = oauth_api,
                                          redirect_uri = APP_URL,
                                          scope = oauth_scope,
                                          d2_session_envir = parent.env(environment())
            ) },
            # This function throws an error if the login is not successful
            error = function(e) {
                flog.info(paste0("User ", input$user_name, " login failed. ", e$message), name = "datimutils")
            }
        )

        if (exists("d2_default_session")) {

            user_input$authenticated  <-  TRUE
            user_input$d2_session  <-  d2_default_session$clone()
            d2_default_session <- NULL

            #Need to check the user is a member of the PRIME Data Systems Group, COP Memo group, or a super user
            user_input$memo_authorized <-
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
                name = "datimutils"
            )


            flog.info(
                paste0(
                    "User ",
                    user_input$d2_session$me$userCredentials$username,
                    " logged in."
                ),
                name = "datimutils"
            )
        }

    })

########## Below this line is your standard server code ##########
### Controls Data Table
    output$mytable <- DT::renderDataTable({

        df <- getMetadata(
            end_point = "organisationUnitGroups",
            fields = "id,name",
            d2_session = user_input$d2_session
        )
    })
})
