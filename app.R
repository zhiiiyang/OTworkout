#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
#source("analyzeWorkout.R")
# Define UI for application that draws a histogram

ui <- dashboardPage(
    dashboardHeader(title = "Zhi's 30-day workout challenge"),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            # A static valueBox
            valueBox(paste0(length(c(1,2)), "/30"), "Completed", 
                     icon = icon("check-square"), color = "aqua"),
            
            # Dynamic valueBoxes
            valueBox(paste0(round(length(c(1,2))/30*100, 1), "%"), 
                     "Progress", color = "red", 
                     icon = icon("battery-half")),
            
            valueBox(sum(as.numeric(c(1,2))), 
                     "Total calories burned", color = "green",
                     icon = icon("dumbbell")),
        )
    )
)

server <- function(input, output) {
    
}

shinyApp(ui, server)
