#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
source("analyzeWorkout.R")

ui <- dashboardPage(
    dashboardHeader(title = "Zhi's 30-day workout challenge"),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            # A static valueBox
            valueBoxOutput("completion"),
            
            valueBoxOutput("progress"),
            
            valueBoxOutput("totalcalories")
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
                 "Progress", color = "red", 
                 icon = icon("battery-half"))
    })
    
    output$totalcalories <- renderValueBox({
        valueBox(sum(as.numeric(records$calories)),
                 "Total calories burned", color = "green",
                 icon = icon("dumbbell"))
    })
}

shinyApp(ui, server)
