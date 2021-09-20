library(shiny)
library(tidyverse)

source("~/Documents/Repos/datimutils/R/OAuth2/oauthShinyModule.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("DHIS 2 OAuth Example Application"),
    shinyOAuthUI("test"),
    "Random Testing"
)


server <- function(input, output,session) {
    shinyOAuthServer("test")
    
    #source("api_keys.R")
    # 
    # OAuthButton <- callModule(shinyOAuth,
    #                           "fitbit_login",
    #                        api_info = api_keys)
    
    # # logic for what happens after a user has drawn their values. Note this will fire on editing again too.
    # observeEvent(OAuthButton(), {
    #     userToken = OAuthButton()
    #     
    #     print(userToken)
    # })
    
}

# Run the application
shinyApp(ui = uiFunc, server = server)
#shinyApp(uiFunc, server)