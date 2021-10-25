library(shiny)
library(tidyverse)
library(httr)
library(xml2)
library(datimutils)
source("~/Documents/Repos/datimutils/inst/templates/oAuthLoginScreen/LoginScreenModule.R")
source("~/Documents/Repos/datimutils/R/oAuthLogin.R")


uiBase <- fluidPage(
    titlePanel("Hello Shiny!"),
    verbatimTextOutput("code"),
    DT::dataTableOutput("mytable")
)

ui <- shinyOAuthUI("oAuth",uiBase)


server <- function(input, output, session) {
    
    shinyOAuthServer("oAuth")
    
    #Users Cod should be nested in this if else statement or could do observer event
    if (authenticated==TRUE){
        
        df=getMetadata(
            end_point = "organisationUnits",
            "organisationUnitGroups.name:eq:District",
            fields = "id,name,level"
        )
        
        output$mytable = DT::renderDataTable({
            df
        })
        
    } #ENDS IF statement 
    
}

shinyApp(ui = ui, server = server)