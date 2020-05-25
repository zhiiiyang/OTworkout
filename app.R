library(shiny)
library(shinyMobile)
library(shinyWidgets)
library(dplyr)

source("analyzeWorkout.R")

shiny::shinyApp(
    ui = f7Page(icon = "icons/icon-128x128.png",
                favicon = "icons/icon-512x512.png",
                manifest = "manifest.json",
        f7TabLayout(
            navbar = f7Navbar(
                title = "Zhi's 30-day Workout Challenge",
                hairline = FALSE,
                shadow = TRUE,
                left_panel = FALSE,
                right_panel = FALSE
            ),
            f7Tabs(
                animated = TRUE,
                #swipeable = TRUE,
                f7Tab(
                    tabName = "Progress",
                    icon = f7Icon("goforward_30"),
                    active = TRUE,
                    f7Shadow(
                        intensity = 10,
                        hover = TRUE,
                        f7SocialCard(
                            author_img = "profile.jpg",
                            author = "Zhi",
                            date = Sys.Date(),
                            plotOutput("Plot1", height = "250px"),
                            footer = tagList(
                                f7Button(label = paste(30 - nrow(records), "days left"), color = "blue",
                                         shadow = TRUE, rounded = TRUE, size = "small", fill = FALSE),
                                f7Button(label = "Time", 
                                         color = ifelse(mean(records$time)<1000/30, "pink", "green"),
                                         shadow = TRUE, rounded = TRUE, size = "small", 
                                         fill = ifelse(mean(records$time)<1000/30, TRUE, FALSE)),
                                f7Button(label = "Calories", 
                                         color = ifelse(mean(records$calories)<7000/30,"pink", "green"),
                                         shadow = TRUE, rounded = TRUE, size = "small", 
                                         fill = ifelse(mean(records$calories)<7000/30, TRUE, FALSE))
                            )
                        ) 
                    ),
                    f7Card(
                        paste0("Completed ", round(length(issues_closed)/30*100), "% of 30 workouts"),
                        f7Progress(id = "p1", value = length(issues_closed)/30*100, color = "green")
                    ),
                    f7Card(
                        paste0("Spent ", round(sum(records$time)/10), "% of 1,000 mins"),
                        f7Progress(id = "p2", value = sum(records$time)/10, color = "lightblue"),
                    ),
                    f7Card(
                        paste0("Burned ", round(sum(records$calories)/70), "% of 7,000 calories (~ 2lb fat)"),
                        f7Progress(id = "p3", value = sum(records$calories)/70, color = "red")
                    ),
                    f7Card(
                        paste0("Average Heart Rate Zone ", 
                               round(mean(records$hr)/189*100), "% : ",
                               ifelse(mean(records$hr)/189*100 > 70, 
                                                                  "Aerobic ", "Weight control ")),
                        f7Progress(id = "p4", value = mean(records$hr)/189*100, 
                                   color = ifelse(mean(records$hr)/189*100 > 70, "orange", "blue")),
                    ),
                    f7Card(
                        paste0("Maximum Heart Rate Zone ", 
                               round(mean(records$maxhr)/189*100), "% : ",
                               ifelse(mean(records$maxhr)/189*100 > 84, 
                                                                  "Anaerobic ", "Aerobic ")),
                        f7Progress(id = "p5", value = mean(records$maxhr)/189*100, 
                                   color = ifelse(mean(records$hr)/189*100 > 84, "red", "orange")),
                    )
                ),
                f7Tab(
                    tabName = "Today's task",
                    icon = f7Icon("calendar_today"),
                    f7Card(
                        title = "Completion status", 
                        f7Button(label = ifelse(todaystatus=="no",
                                                "Please uploaded your workout!",
                                                "Well done!"), 
                                 color = ifelse(todaystatus=="no",
                                                "red",
                                                "green"),
                                 fill = FALSE),
                        br(),
                        f7Button(inputId = "go", "Show me more workouts!", 
                                 color = "lightblue")
                    ) %>% f7Skeleton(effect = "fade", duration = 1)
                    
                ), 
                
                f7Tab(
                    tabName = "Calories converter",
                    icon = f7Icon("zoom_in"),
                    active = FALSE,
                    f7Select(
                        inputId = "food",
                        label = "Choose a food",
                        choices = calories$food,
                        width = "100%"
                    ),
                    f7Shadow(
                        intensity = 10,
                        f7Card(
                            title = textOutput("frequency"),
                            plotOutput("Plot2"),
                            height = "100%"
                        )
                    )
                )
            )
        )
    ),
    server = function(input, output) {
        
        output$Plot1 <- renderImage({
            # A temp file to save the output.
            # This file will be removed later by renderImage
            outfile <- file.create("ring.png")
            
            # Generate the PNG
            png("ring.png",width = 380*8, height = 200*8, 
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
                theme(legend.position = "none",
                      plot.margin = unit(c(-0.3,-0.3,-0.3,-0.3), "cm"))
            print(p)
            dev.off()
            
            # Return a list containing the filename
            list(src = "ring.png",
                 contentType = 'image/png',
                 width = 380,
                 height = 200)
        }, deleteFile = TRUE)
        
        output$progress <- renderText({
            length(issues_closed)/30
        })
        
        output$frequency <- renderText({
            freq <- sum(as.numeric(records$calories))/calories$calories[which(calories$food==input$food)]
            paste0("After ", nrow(records), " workouts, you have burned calories same as ", round(freq, 1), " units")
        })
        
        output$Plot2 <- renderImage({
            list(src = calories$imgs[which(calories$food==input$food)],
                 contentType = 'image/png',
                 width = 300,
                 height = 300)
        }, deleteFile = FALSE)
    }
)