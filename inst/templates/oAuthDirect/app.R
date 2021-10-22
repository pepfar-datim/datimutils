library(shiny)
library(tidyverse)
library(httr)
library(xml2)
library(datimutils)
source("~/Documents/Repos/datimutils/inst/templates/oAuthDirect/OAuthModule.R")
source("~/Documents/Repos/datimutils/R/oAuthLogin.R")


uiBase <- fluidPage(
    titlePanel("Hello Shiny!"),
    verbatimTextOutput("code"),
    DT::dataTableOutput("mytable")
)

ui <- shinyOAuthUI("oAuth",uiBase)


server <- function(input, output, session) {
    
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