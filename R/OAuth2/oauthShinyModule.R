library(httr)
#Prep for local or deployed app

if (interactive()) {
  # testing url
  options(shiny.port = 8100)
  APP_URL <- "http://localhost:8100/" 
} else {
  # deployed URL
  APP_URL <- "https://servername/path-to-app" #This will be your shiny server path
}





# Note that secret is not really secret, and it's fine to include inline
# Copy information directly from the dhis2.org web settings
app <- oauth_app("OAuth2 Demo Client", #dhis2 = Name
                 key = "demo",         #dhis2 = Client ID
                 secret = "ce57f2d89-9910-01a7-d0d7-22dfbc5ae8b", #dhis2 = Client Secret
                 redirect_uri = APP_URL  
)

# Create endpoints with oauth_endpoint()
api <- oauth_endpoint("requestToken","authorize", "accessToken",
                      base_url = "https://play.dhis2.org/2.36.3/uaa/oauth"
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






#' Allows the easy retreival of a token for using apis in your shiny apps.
#' @param id the id you will use to keep track of this component in your app
#' @return A button that a used presses to login to your given authentication service.
#' @param defaultUI
#' @export
#' @examples
#' shinydrawrUI('myrecorder')

shinyOAuthUI <- function(id) {
  ns <- NS(id)
  #return("Test")
  fluidPage(
    fluidRow(
      column(12, uiOutput(ns("First_UI")))
    )
  )
  

  #Grab the external javascript and css
  #OAuthrjs <- .get_script("OAuthr.js", "js")
  #shinyOAuthjs <- .get_script("shinyOAuth.js", "js")
  
  # tagList(
  #   singleton(
  #     tags$head( #load external scripts.
  #       tags$script(HTML(OAuthrjs)),
  #       tags$script(HTML(shinyOAuthjs))
  #     )
  #   ),
  #   tags$button(id = ns("OAuthButton"), "Login Button")
  #   
  # ) #end tag list.
  
  
  
}


#' Server side component. You supply this with your api credentials.
#'
#' @param input you can ignore this as it is taken care of by shiny
#' @param output you can ignore this as it is taken care of by shiny
#' @param session you can ignore this as it is taken care of by shiny
#' @param api_info A named list of the various api values you need to authenticate. "key", "id", "secret", "url", "token_url" 'https://www.fitbit.com/oauth2/authorize'
#' @param api_key your apps personal api key.
#' @param scope a vector of what you're requesting access to in the API. See the given api docs for examples.
#' @export

shinyOAuthServer <- function(id) { 
  
  #moduleServer(id, function(input, output, session, api_info, response_type = "code"){
  moduleServer(id, function(input, output, session){
    
    output$First_UI <- renderUI({
      
      ns <- session$ns
      
      params <- parseQueryString(isolate(session$clientData$url_search))
      observe({print(session$clientData$url_search)})
      
      if (!has_auth_code(params)) {
        return()
      }
      
      # Manually create a token
      token <- oauth2.0_token(
        app = app,
        endpoint = api,
        credentials = oauth2.0_access_token(api, app, params$code),
        cache = FALSE
      )
      print(token)
      
      resp <- GET("https://play.dhis2.org/2.36.3/uaa/oauth/token/grant_type=authorization_code/", config(token = token))
      # TODO: check for success/failure here
      
      output$code <- renderText(content(resp, "text"))
      
      uiFunc <- function(req) {
        if (!has_auth_code(parseQueryString(req$QUERY_STRING))) { #IF does NOT have token take them to
          url <- oauth2.0_authorize_url(api, app, scope = scope)
          redirect <- sprintf("location.replace(\"%s\");", url)
          tags$script(HTML(redirect))
        } else { 
          
          #ui
          #shinyUI(defaultUI)

          shinyUI(fluidPage(

            # Application title
            titlePanel("Old Faithful Geyser Data"),

            # Sidebar with a slider input for number of bins
            sidebarLayout(
              sidebarPanel(
                sliderInput("bins",
                            "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30)
              ),

              # Show a plot of the generated distribution
              mainPanel(
                plotOutput("distPlot")
              )
            )
          ))
        }

      }
    })
    

  })
  
  #key_secret_code <- .to_base_64(paste0(api_info$key, ":", api_info$secret))
  
  # #Send over a message to javascript.
  # observe({ session$sendCustomMessage(
  #   type = "initialize_button",
  #   message = list(
  #     dom_target = session$ns("OAuthButton"),
  #     main_url = api_info$OAuth_url,
  #     api_key = api_info$key,
  #     scope = api_info$scope,
  #     response_type = response_type,
  #     id  = session$ns(""))
  # )
  # })
  
  # # The user's api token in string format.
  # result <- reactive({
  #   
  #   token_request <- getToken(
  #     auth_code = input$code ,
  #     redirect_uri = api_info$redirect_uri,
  #     key = api_info$key,
  #     token_url = api_info$token_url,
  #     key_secret_code = key_secret_code
  #   )
  #   
  #   
  #   token_request$access_token
  # })
  # 
  # 
  # return(result)
  
  ###############################################################
  
  
}