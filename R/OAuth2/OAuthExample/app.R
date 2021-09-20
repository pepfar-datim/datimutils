library(shiny)
library(tidyverse)




# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Let's query fitbit"),
    shinyOAuthUI("fitbit_login")
)


server <- function(input, output) {
    source("api_keys.R")
    
    OAuthButton <- callModule(shinyOAuth,
                             "fitbit_login",
                             api_info = api_keys)
    
    # # logic for what happens after a user has drawn their values. Note this will fire on editing again too.
    # observeEvent(OAuthButton(), {
    #     userToken = OAuthButton()
    #     
    #     print(userToken)
    # })
    
}

# Run the application
shinyApp(ui = ui, server = server, options = c("port" = 1410))