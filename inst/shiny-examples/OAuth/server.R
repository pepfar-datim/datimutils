pacman::p_load(shiny, shinyjs, shinyWidgets, magrittr, dplyr,
               datimvalidation, ggplot2, datimutils,
               futile.logger, paws, datapackr, scales,
               DT, purrr, rpivotTable, waiter,
               flextable, officer, gdtools, digest, fansi)

# js ----
# allows for using the enter button
jscode_login <- '$(document).keyup(function(e) {
    var focusedElement = document.activeElement.id;
    console.log(focusedElement);
    if (e.key == "Enter" && focusedElement == "user_name") {
    $("#password").focus();
    } else if (e.key == "Enter" && focusedElement == "password") {
    $("#login_button").click();
    }
});'

#Set the maximum file size for the upload file
options(shiny.maxRequestSize = 150 * 1024 ^ 2)
#Allow unsanitized error messages
options(shiny.sanitize.errors = FALSE)
#Initiate logging
logger <- flog.logger()

if (!file.exists(Sys.getenv("LOG_PATH"))) {
    file.create(Sys.getenv("LOG_PATH"))
}

flog.appender(appender.console(), name = "datapack")

################ OAuth Client information #####################################
if (interactive()) {
    # testing url
    options(shiny.port = 3123)
    APP_URL <- "http://127.0.0.1:3123/"# This will be your local host path
} else {
    # deployed URL
    APP_URL <- Sys.getenv("APP_URL") #This will be your shiny server path
}

{
    
    oauth_app <- httr::oauth_app(Sys.getenv("OAUTH_APPNAME"),
                                 key = Sys.getenv("OAUTH_KEYNAME"),        # dhis2 = Client ID
                                 secret = Sys.getenv("OAUTH_SECRET"), #dhis2 = Client Secret
                                 redirect_uri = APP_URL
    )
    
    oauth_api <- httr::oauth_endpoint(base_url = paste0(Sys.getenv("BASE_URL"),"uaa/oauth"),
                                      request=NULL,# Documentation says to leave this NULL for OAuth2
                                      authorize = "authorize",
                                      access="token"
    )
    
    oauth_scope <- "ALL"
}

has_auth_code <- function(params) {
    
    return(!is.null(params$code))
}



shinyServer(function(input, output, session) {
    
    validation_results <- reactive({ validate() }) # nolint
    
    ready <- reactiveValues(ok = FALSE)
    
    
    user_input <- reactiveValues(authenticated = FALSE,
                                 status = "",
                                 d2_session = NULL,
                                 memo_authorized = FALSE,
                                 uuid = NULL)
    
    
    observeEvent(input$file1, {
        shinyjs::show("validate")
        shinyjs::enable("validate")
        ready$ok <- FALSE
    })
    
    observeEvent(input$validate, {
        shinyjs::disable("file1")
        shinyjs::disable("validate")
        ready$ok <- TRUE
    })
    
    observeEvent(input$reset_input, {
        shinyjs::reset("side-panel")
        shinyjs::enable("file1")
        shinyjs::disable("validate")
        shinyjs::disable("downloadDataPack")
        shinyjs::disable("download_messages")
        shinyjs::disable("send_paw")
        shinyjs::disable("downloadValidationResults")
        shinyjs::disable("compare")
        ready$ok <- FALSE
    })
    

    
    

    
    observeEvent(input$logout, {
        req(input$logout)
        # Gets you back to the login without the authorization code at top
        updateQueryString("?",mode="replace",session=session)
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
    
    output$ui <- renderUI({
        
        if (user_input$authenticated == FALSE) {
            ##### UI code for login page
            fluidPage(
                fluidRow(
                    column(width = 2, offset = 5,
                           br(), br(), br(), br(),
                           uiOutput("uiLogin")
                    )
                )
            )
        } else {
            uiOutput("authenticated")
        }
    })
    
    
    # Username and password text fields, login button
    output$uiLogin <- renderUI({
        
        wellPanel(fluidRow(
            #img(src = "pepfar.png", align = "center"),
            tags$head(tags$script(HTML(jscode_login))), # enter button functionality for login button
            tags$div(HTML('<center><img src="pepfar.png"></center>')),
            h4("Welcome to the DataPack Validation App. Please login with your DATIM credentials:")
        ),
        fluidRow(
            actionButton("login_button_oauth","Log in with DATIM"),
            uiOutput("ui_hasauth"),
            uiOutput("ui_redirect")
        ),
        fluidRow(
            tags$hr(),
            tags$div(HTML("<ul><li><h4>Please be sure you fully populate the PSNUxIM",
                          " tab when receiving a new DataPack. Consult <a href = ",
                          "\"https://apps.datim.org/datapack-userguide/\" target = ",
                          "\"blank\" > the user guide</a> for further information!",
                          "</h4></li><li><h4>See the latest updates to the app <a href =",
                          "\"https://github.com/pepfar-datim/datapackr-app/blob/master/CHANGELOG.md\"",
                          "target = \"blank\">here.</h4></a></li></ul>"))
        ),
        tags$hr(),
        fluidRow(HTML(getVersionInfo())))
    })
    
    output$authenticated <- renderUI({
        wiki_url <- a("Datapack User Guide",
                      href = "https://apps.datim.org/datapack-userguide/",
                      target = "_blank")
        
        fluidPage(
            tags$head(tags$style(".shiny-notification {
                             position: fixed;
                             top: 10%;
                             left: 33%;
                             right: 33%;}")),
            use_waiter(),
            sidebarLayout(
                sidebarPanel(
                    shinyjs::useShinyjs(),
                    id = "side-panel",
                    tagList(wiki_url),
                    tags$hr(),
                    fileInput(
                        "file1",
                        "Choose DataPack (Must be XLSX!):",
                        accept = c("application/xlsx",
                                   ".xlsx"),
                        width = "240px"
                    ),
                    actionButton("validate", "Validate"),
                    tags$hr(),
                    selectInput("downloadType", "Download type", NULL),
                    downloadButton("downloadOutputs", "Download"),
                    tags$hr(),
                    actionButton("send_paw", "Send to PAW"),
                    tags$hr(),
                    div(style = "display: inline-block; vertical-align:top; width: 80 px;",
                        actionButton("reset_input", "Reset inputs")),
                    div(style = "display: inline-block; vertical-align:top; width: 80 px;",
                        actionButton("logout", "Logout"))
                ),
                
                mainPanel(tabsetPanel(
                    id = "main-panel",
                    type = "tabs",
                    tabPanel("Messages", tags$ul(uiOutput("messages"))),
                    tabPanel("Analytics checks", tags$div(uiOutput("analytics_checks"))),
                    tabPanel("Indicator summary", dataTableOutput("indicator_summary"),
                             tags$h4("Data source: Main DataPack tabs")),
                    tabPanel("SNU-level summary",
                             dataTableOutput("snu_summary"),
                             tags$h4("Data source: Main DataPack tabs")),
                    tabPanel("Validation rules",
                             dataTableOutput("vr_rules"),
                             tags$h4("Data source: PSNUxIM tab")),
                    tabPanel("HTS Summary Chart",
                             fluidRow(column(width = 12, div(style = "height:700px", plotOutput("modality_summary")))),
                             fluidRow(column(width = 12, tags$h4("Data source: PSNUxIM tab")))),
                    tabPanel("HTS Summary Table",
                             dataTableOutput("modality_table"),
                             tags$h4("Data source: PSNUxIM tab")),
                    tabPanel("HTS Yield",
                             fluidRow(column(width = 12, div(style = "height:700px", plotOutput("modality_yield")))),
                             fluidRow(tags$h4("Data source: PSNUxIM tab"))),
                    tabPanel("HTS Recency",
                             dataTableOutput("hts_recency"),
                             tags$h4("Data source: PSNUxIM tab")),
                    tabPanel("VLS Testing",
                             fluidRow(column(width = 12, div(style = "height:700px", plotOutput("vls_summary")))),
                             fluidRow(column(width = 12, tags$h4("Data source: PSNUxIM tab")))),
                    tabPanel("Epi Cascade Pyramid",
                             pickerInput("epiCascadeInput", "SNU1",
                                         choices = "",
                                         options = list(`actions-box` = TRUE), multiple = T),
                             fluidRow(column(width = 12, div(style = "height:700px", plotOutput("epi_cascade")))),
                             fluidRow(tags$h4("Data source: SUBNATT/IMPATT data & PSNUxIM tab"))),
                    tabPanel("KP Cascade Pyramid",
                             pickerInput("kpCascadeInput", "SNU1",
                                         choices = "",
                                         options = list(`actions-box` = TRUE), multiple = T),
                             fluidRow(column(width = 12, div(style = "height:700px", plotOutput("kp_cascade")))),
                             fluidRow(tags$h4("Data source: Data source: SUBNATT/IMPATT data & PSNUxIM tab"))),
                    tabPanel("PSNUxIM Pivot",
                             fluidRow(column(width = 12, div(rpivotTable::rpivotTableOutput({"pivot"})))), # nolint
                             fluidRow(tags$h4("Data source: PSNUxIM tab"))),
                    tabPanel(
                        "Memo Tables",
                        fluidRow(
                            pickerInput(
                                "memo_pivot_style",
                                label = "Table Style",
                                choices = c("Prioritization", "By Agency", "By Partner", "Comparison"),
                                selected = "Prioritization"
                            )
                        ),
                        fluidRow(column(width = 12,
                                        div(dataTableOutput({"memo_compare"})))), # nolint
                        fluidRow(tags$h4("Data source: PSNUxIM tab & DATIM")))
                    
                ))
            ))
    })
    
    output$ui_redirect = renderUI({
        #print(input$login_button_oauth) useful for debugging
        if(!is.null(input$login_button_oauth)){
            if(input$login_button_oauth>0){
                url <- httr::oauth2.0_authorize_url(oauth_api, oauth_app, scope = oauth_scope)
                redirect <- sprintf("location.replace(\"%s\");", url)
                tags$script(HTML(redirect))
            } else NULL
        } else NULL
    })
    
    ### Login Button oauth Checks
    observeEvent(input$login_button_oauth > 0,{
        
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
                                          redirect_uri= APP_URL,
                                          scope = oauth_scope,
                                          d2_session_envir = parent.env(environment())
            ) },
            # This function throws an error if the login is not successful
            error = function(e) {
                flog.info(paste0("User ", input$user_name, " login failed. ", e$message), name = "datapack")
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
                name = "datapack"
            )
            sendEventToS3(NULL, "LOGIN", user_input = user_input)
            
            flog.info(
                paste0(
                    "User ",
                    user_input$d2_session$me$userCredentials$username,
                    " logged in."
                ),
                name = "datapack"
            )
        }
        
    })
    
})
