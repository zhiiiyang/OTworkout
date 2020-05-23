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
library(metathis)
source("analyzeWorkout.R")

addResourcePath(prefix = 'pics', directoryPath = 'www/')

ui <- fluidPage(
    h3("Zhi's 30-day Workout challenge", align = "center"),
    
    tags$head(tags$link(rel="shortcut icon", href="pics/favicon.ico")),
    
    shinyWidgets::useShinydashboard(),
    
    plotOutput("completion"),
    
    valueBoxOutput("progress"),
    
    valueBoxOutput("totalcalories"),
    
    valueBoxOutput("totaltime"),
    
    valueBoxOutput("hr")
)

server <- function(input, output) {
    output$completion <- renderValueBox({
        valueBox(paste0(length(issues_closed), "/30"), "Completed", 
                 icon = icon("check-square"), color = "aqua")
    })
    
    output$completion <- renderPlot({
        ggplot(data = data) +
            geom_rect(aes(ymax=ymax-0.002, ymin=ymin+0.002, xmax=3, xmin=2, fill=category)) +
            geom_text( x=-1, y = 0, label = paste0(nrow(records),"/30"), size=10) + 
            scale_fill_manual(values = c(hcl.colors(30, palette = "Temp", rev = TRUE)[1:nrow(records)],
                                         rep("gray95", 30 - nrow(records))))+
            coord_polar(theta="y") +
            xlim(c(-1, 4)) +
            theme_void() +
            theme(legend.position = "none")
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
