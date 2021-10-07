library(shiny)
library(tidyverse)
library(httr)
library(xml2)
source("~/Documents/Repos/datimutils/R/OAuthModule.R")



if (interactive()) {
    # testing url
    options(shiny.port = 8100)
    APP_URL <<- "http://127.0.0.1:8100/"
} else {
    # deployed URL
    APP_URL <<- "https://rstudio-connect.testing.ap.datim.org/OAuth2/" #This will be your shiny server path
}

app <<- oauth_app("OAuth2 Demo Client", #dhis2 = Name
                 key = "demo",         #dhis2 = Client ID
                 secret = "68bd4f81d-9b0d-f256-5d9c-0234393e7ae", #dhis2 = Client Secret
                 redirect_uri = APP_URL #"http://127.0.0.1:8100/"
)

api <<- oauth_endpoint(base_url = "https://play.dhis2.org/2.36.3/uaa/oauth",
                      request=NULL,#Documentation says to leave this NULL for OAuth2 
                      authorize = "authorize",
                      access="token"
                   ) 

scope <<- "ALL"


uiBase <- fluidPage(
    titlePanel("Hello Shiny!"),
    verbatimTextOutput("code"),
    DT::dataTableOutput("mytable")
)

ui <- shinyOAuthUI("oAuth",uiBase,app,api,scope)


server <- function(input, output, session,scope) {

    shinyOAuthServer("oAuth")
    
    df=getMetadata(
        end_point = "organisationUnits",
        "organisationUnitGroups.name:eq:District",
        fields = "id,name,level"
    )
    
    
    output$mytable = DT::renderDataTable({
        df
    })
    
    #Alternate if the above is deemed unusable
    # token=shinyOAuthServer("oAuth")
    # 
    # # form url
    # base_url="play.dhis2.org/2.36.3/"
    # url <- utils::URLencode(URL = paste0(base_url, "api", "/me"))
    # handle <- httr::handle(base_url)
    # 
    # 
    # #curl command to get the above 
    # resp <- GET(url, config(token = token))
    # # TODO: check for success/failure here
    # 
    # output$code <- renderText(content(resp, "text"))
    
}

shinyApp(ui = ui, server = server)