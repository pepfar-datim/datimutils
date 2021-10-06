library(shiny)
library(dplyr)
library(ggplot2)

# ### Shiny Module fo OAuth login
# 
# # params is a list object containing the parsed URL parameters. Return TRUE if
# # based on these parameters, it looks like auth codes are present that we can
# # use to get an access token. If not, it means we need to go through the OAuth
# # flow.
has_auth_code <- function(params) {

  return(!is.null(params$code))

}



# innerModUI <- function(id) {
#   ns <- NS(id)
#   
#   fluidPage(fluidRow(
#     uiOutput(ns("inner_slider")),
#     plotOutput(ns("inner_plot"))
#   ))
# }

# innerMod <- function(input, output, session) {
#   output$inner_slider <- renderUI({
#     sliderInput(session$ns("slider2"),
#                 label = "inner module slider",
#                 min = round(min(mtcars$mpg)), 
#                 max = round(max(mtcars$mpg)),
#                 value = c(min(mtcars$mpg), max(mtcars$mpg)), step = 1)
#   })
  
#   output$inner_plot <- renderPlot({
#     req(input$slider2)
#     data <- filter(mtcars, between(mpg, input$slider2[1], input$slider2[2]))
#     ggplot(data, aes(mpg, wt)) + geom_point()
#   })
# }


### UI Section
shinyOAuthUI <- function(id) {
  ns <- NS(id)
  fluidPage(fluidRow(
    uiOutput(ns("oAuth_UI"))#,
    #plotOutput(ns("outer_plot")),
    #innerModUI(ns("inner"))
  ))
}

### Server Section
shinyOAuthServer <- function(input, output, session) {
  #callModule(innerMod, "inner")
  
  output$oAuth_UI <- renderUI({
    
     ns <- session$ns
     
     
     url <- oauth2.0_authorize_url(api, app, scope = scope)
     redirect <- sprintf("location.replace(\"%s\");", url)
     tags$script(HTML(redirect))
     
    # 
    # params <- parseQueryString(isolate(session$clientData$url_search))
    # 
    # if (!has_auth_code(params)) {
    #   return()
    # }
  
    
    # sliderInput(session$ns("slider1"),
    #             label = "outer module slider",
    #             min = round(min(mtcars$mpg)),
    #             max = round(max(mtcars$mpg)),
    #             value = c(min(mtcars$mpg), max(mtcars$mpg)), step = 1)

    # uiFunc <- function(req) {
    # 
    #   if (!has_auth_code(parseQueryString(req$QUERY_STRING))) { #IF does NOT have token take them to
    #     url <- oauth2.0_authorize_url(api, app, scope = scope)
    #     redirect <- sprintf("location.replace(\"%s\");", url)
    #     tags$script(HTML(redirect))
    #   } else {
    #     ui
    #   }
    # }

   })
  
  # output$outer_plot <- renderPlot({
  #   req(input$slider1)
  #   data <- filter(mtcars, between(mpg, input$slider1[1], input$slider1[2]))
  #   ggplot(data, aes(mpg, wt)) + geom_point()
  # })
}

# ### Server Section
# shinyOAuthServer <- function(id) { 
#   
#   moduleServer(
#     id,
#     function(input, output, session){
#       output$oAuth_UI <- renderUI({
#       
#       #ns <- session$ns
#       
#       #### Actual UI CODE #####
#       uiFunc  <- function(req) {
#         if (!has_auth_code(parseQueryString(req$QUERY_STRING))) { #IF does NOT have token take them to
#           url <- oauth2.0_authorize_url(api, app, scope = scope)
#           redirect <- sprintf("location.replace(\"%s\");", url)
#           tags$script(HTML(redirect))
#         } else {
#           ui
#         }
#       }
#       
#       
#       
#       
#       ######################################################################
#       params <- parseQueryString(isolate(session$clientData$url_search))
#       observe({print(session$clientData$url_search)})
#       
#       if (!has_auth_code(params)) {
#         return()
#       }
#       
#       # Manually create a token
#       token <- oauth2.0_token(
#         app = app,
#         endpoint =api, 
#         scope = scope,
#         use_basic_auth = TRUE,
#         oob_value=APP_URL, #"http://127.0.0.1:8100/", 
#         cache = FALSE,
#         credentials = oauth2.0_access_token(endpoint = api,
#                                             app = app,
#                                             code = params$code,
#                                             use_basic_auth = TRUE)
#       )
#       
# 
#     })
#     
#   })
# }