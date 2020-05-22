#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(golem)
source("analyzeWorkout.R")

ui <- dashboardPage(
    
    dashboardHeader(title = "Zhi's 30-day workout challenge"),
    dashboardSidebar(),
    dashboardBody(
        tags$head(tags$link(rel="shortcut icon", href="www/favicon.ico")),
        
        fluidRow(
            # A static valueBox
            valueBoxOutput("completion"),
            
            valueBoxOutput("progress"),
            
            valueBoxOutput("totalcalories"),
            
            valueBoxOutput("totaltime"),
            
            valueBoxOutput("hr")
        )
    )
)

server <- function(input, output) {
    output$completion <- renderValueBox({
        valueBox(paste0(length(issues_closed), "/30"), "Completed", 
                 icon = icon("check-square"), color = "aqua")
    })
    
    output$progress <- renderValueBox({
        valueBox(paste0(round(length(issues_closed)/30*100, 1), "%"), 
                 "Progress", color = "teal", 
                 icon = icon("battery-half"))
    })
    
    output$totalcalories <- renderValueBox({
        valueBox(sum(as.numeric(records$calories)),
                 "Total calories burned", color = "green",
                 icon = icon("burn"))
    })
    
    output$totaltime <- renderValueBox({
        valueBox(sprintf("%shr %smin", 
                         round(sum(records$time) %/% 60),
                         round(sum(records$time) %% 60)),
                 "Total time spent", color = "yellow",
                 icon = icon("clock"))
    })
    
    output$hr <- renderValueBox({
        valueBox(sprintf("%s \u00B1 %s", 
                         round(mean(as.numeric(records$hr))),
                         round(sd(as.numeric(records$hr)))),
                 "Average heart rate", color = "red",
                 icon = icon("heart"))
    })
    

}

shinyApp(ui, server)
