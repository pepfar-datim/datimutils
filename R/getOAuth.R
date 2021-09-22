library(shiny)
library(httr)

# OAuth setup --------------------------------------------------------

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

ui <- fluidPage(
  # Your regular UI goes here, for when everything is properly auth'd
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
    #ui
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

server <- function(input, output, session) {
  
  params <- parseQueryString(isolate(session$clientData$url_search))
  observe({print(session$clientData$url_search)})
  print(paste("Parameters equal",params))
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
  
### Visual testing output
    output$distPlot <- renderPlot({
      
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
      
    })
    
}

# Note that we're using uiFunc, not ui!
shinyApp(uiFunc, server)


# Sat Sep 11 10:33:56 2021 ------------------------------
#Easiest way to make the client is by logging into DHIS2 and creating it via settings
#NOTE will have to redone daily since dhis2 pointed to currently is reset nightly

#Step 2 
#Go here
#https://play.dhis2.org/2.36.3/uaa/oauth/authorize?client_id=demo&response_type=code&redirect_uri=http://localhost:8100/




# Thu Sep 16 17:59:25 2021 ------------------------------
#Without Shiny 
# oauth2.0_token(
#   endpoint = oauth_endpoint("requestToken","authorize", "accessToken",
#                             base_url = "https://play.dhis2.org/2.36.3/uaa/oauth"),
#   app = oauth_app("OAuth2 Demo Client",
#                   key = "demo",
#                   secret = "b18b6560d-0fe3-02ad-6322-25ff2813aae"),
#   #redirect_uri = "http://localhost:1410/"),
#   scope = c("ALL"),
#   cache = FALSE
# )