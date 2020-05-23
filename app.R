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
    tags$head(tags$link(rel="shortcut icon", href="pics/favicon.ico")),
    
    shinyWidgets::useShinydashboard(),
    
    column(12,
           imageOutput("completion", height = "100%", width = "100%"),
           align = "center"),
    
    valueBoxOutput("progress"),
    
    valueBoxOutput("totalcalories"),
    
    valueBoxOutput("totaltime"),
    
    valueBoxOutput("hr")
)

server <- function(input, output) {
    output$completion <- renderImage({
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext = '.png')
        
        # Generate the PNG
        png(outfile,width = 200*8, height = 200*8, 
            res = 72*20)
        p <- ggplot(data = data) +
            geom_rect(aes(ymax=ymax-0.002, ymin=ymin+0.002, xmax=3, xmin=2, fill=category)) +
            geom_text( x=0, y = 0, 
                       label = paste0(nrow(records),"\nCompleted"), 
                       size=1.5) + 
            scale_fill_manual(values = c(hcl.colors(30, palette = "Temp", rev = TRUE)[1:nrow(records)],
                                         rep("gray95", 30 - nrow(records))))+
            coord_polar(theta="y") +
            xlim(c(0, 3)) +
            theme_void() +            
            ggtitle("Your progress so far")+
            theme(legend.position = "none",
                  plot.title = element_text(size = 4, hjust = 0.5, vjust = -1),
                  plot.margin = unit(c(-0.1,0,0,0), "cm"))
        print(p)
        dev.off()
        
        # Return a list containing the filename
        list(src = outfile,
             contentType = 'image/png',
             width = 300,
             height = 300)
    }, deleteFile = TRUE)
    
    output$progress <- renderValueBox({
        valueBox(tags$p(paste0(round(length(issues_closed)/30*100, 1), "%"),
                        style = "font-size: 80%;"),
                 tags$p("Progress", style = "font-size: 120%;"), 
                 color = "teal", 
                 icon = icon("battery-half"))
    })
    
    output$totalcalories <- renderValueBox({
        valueBox(tags$p(sum(as.numeric(records$calories)),
                       style = "font-size: 80%;"),
                 tags$p("Total calories burned", style = "font-size: 120%;"),
                 color = "green",
                 icon = icon("burn"))
    })
    
    output$totaltime <- renderValueBox({
        valueBox(tags$p(sprintf("%shr %smin", 
                                round(sum(records$time) %/% 60),
                                round(sum(records$time) %% 60)),
                        style = "font-size: 80%;"),
                 tags$p("Total time spent", style = "font-size: 120%;"),
                 color = "yellow",
                 icon = icon("clock"))
    })
    
    output$hr <- renderValueBox({
        valueBox(tags$p(sprintf("%s \u00B1 %s", 
                                round(mean(as.numeric(records$hr))),
                                round(sd(as.numeric(records$hr)))),
                        style = "font-size: 80%;"),
                 tags$p("Average heart rate", style = "font-size: 120%;"),
                 color = "red",
                 icon = icon("heart"))
    })
    
    

}

shinyApp(ui, server)
