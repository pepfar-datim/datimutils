library(shiny)
library(tidyverse)
library(httr)
library(xml2)
library(datimutils)
source("~/Documents/Repos/datimutils/R/OAuthModule.R")
source("~/Documents/Repos/datimutils/R/oAuthLogin.R")



if (interactive()) {
    # testing url
    options(shiny.port = 8100)
    APP_URL <<- "http://127.0.0.1:8100/"
} else {
    # deployed URL
    APP_URL <<- "https://rstudio-connect.testing.ap.datim.org/content/92" #This will be your shiny server path
}

app <<- oauth_app("OAuth2 Demo Client", #dhis2 = Name
                 key = "demo",         #dhis2 = Client ID
                 secret = "96eb2fc53-a0af-2f0b-509c-15ba027cded", #dhis2 = Client Secret
                 redirect_uri = APP_URL #"http://127.0.0.1:8100/"
)

api <<- oauth_endpoint(base_url = "https://play.dhis2.org/2.36.4/uaa/oauth",
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
    
}

shinyApp(ui = ui, server = server)