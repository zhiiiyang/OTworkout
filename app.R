library(shiny)
library(shinyMobile)
library(shinyWidgets)
library(dplyr)
library(echarts4r)
library(tesseract)
library(stringr)
library(zeallot)
library(gh)
library(ggplot2)
library(lubridate)


source("analyzeWorkout.R")


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
            swipeable = TRUE,
            animated = FALSE,
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
                        div(img(src = "ring.png", width = "100%"), style="text-align: center;"),
                        footer = tagList(
                            f7PopoverTarget(f7Button(label = paste(30 - length(unique(records$issue)), "days left"), 
                                                     color = ifelse(as.numeric(today-firstday + 1) - length(unique(records$issue)) < 2, "lightblue", "orange"),
                                                     rounded = TRUE),
                                            targetId = "days"),    
                            
                                f7PopoverTarget(f7Button(label = ifelse(mean(records_by_day$time)<1000/30, "Longer time!", "Time OK!"), 
                                                    color = ifelse(mean(records_by_day$time)<1000/30, "orange", "lightblue"),
                                                    rounded = TRUE),
                                            targetId = "time"),
                            
                            f7PopoverTarget(f7Button(label = ifelse(mean(records_by_day$calories)<7000/30,"More calories!", "Calories OK!"), 
                                                     color = ifelse(mean(records_by_day$calories)<7000/30, "orange", "lightblue"),
                                                     rounded = TRUE),
                                            targetId = "calories"),
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
                    paste0(sprintf("Average Heart Rate Zone [%s%%, %s%%]: ", round(min(records$hr)/189*100), round(max(records$hr)/189*100)),
                           ifelse(mean(records$hr)/189*100 > 70, 
                                                              "Aerobic ", "Weight control ")),
                    f7Progress(id = "p4", value = mean(records$hr)/189*100, 
                               color = ifelse(mean(records$hr)/189*100 > 70, "orange", "blue")),
                ),
                f7Card(
                    paste0(sprintf("Maximum Heart Rate Zone [%s%%, %s%%]: ", round(min(records$maxhr)/189*100), round(max(records$maxhr)/189*100)),
                           ifelse(mean(records$maxhr)/189*100 > 84, 
                                                              "Anaerobic ", "Aerobic ")),
                    f7Progress(id = "p5", value = mean(records$maxhr)/189*100, 
                               color = ifelse(mean(records$hr)/189*100 > 84, "red", "orange")),
                )
            ),
            
            
            f7Tab(
                tabName = "History",
                icon = f7Icon("book"),
                
                f7Card(
                    f7DatePicker(
                        inputId = "date",
                        label = "Choose a date",
                        value = Sys.Date(),
                        minDate = firstday,
                        maxDate = firstday + 30 -1,
                        openIn = "customModal"
                    ),
                    
                    f7Timeline(timeline,
                               sides = TRUE)
                )
            ),
            
            f7Tab(
                tabName = "Today's task",
                icon = f7Icon("calendar_today"),
                f7SocialCard(
                    author_img = "profile.jpg",
                    author = "Zhi",
                    date = Sys.Date(),
                    liquid %>% 
                        e_charts() %>% 
                        e_liquid(value, color = color,
                                 label = list(fontSize = 50),
                                 radius = "90%"),
                    footer = tagList(
                        f7Link(label = "Submit workout", src = sprintf("https://github.com/zhiiiyang/OTworkout/issues/%s", length(unique(records$issue))+2), external = TRUE),
                        f7Link(label = "Start workout", src = "https://apps.apple.com/us/app/orangetheory-fitness/id1424351827", external = TRUE)
                    )
                ),
                

                f7Card(
                    f7Button(inputId = "suggestions", "Click here for suggestions!",                                 
                             color = "lightblue")
                )
            ), 
            
            f7Tab(
                tabName = "Calories converter",
                icon = f7Icon("zoom_in"),
                active = FALSE,
                f7Shadow(
                    intensity = 10,  
                    f7Card(
                        f7SmartSelect(
                            inputId = "food",
                            label = "Choose a food",
                            choices = calories$food,
                            openIn = "sheet"
                        )
                    )
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
)

server = function(input, output, session) { 
    
    output$frequency <- renderText({
        freq <- sum(as.numeric(records$calories))/calories$calories[which(calories$food==input$food)]
        paste0("After ", nrow(records), " workouts, you have burned calories same as ", round(freq, 1), " units")
    })
    
    
    observeEvent(input$suggestions, {
        f7Notif(
            title = "For the next workout",
            icon = f7Icon("hand_point_right"),
            text = sprintf("You need to work out for > %s mins and burn > %s calories", 
                           round((1000-sum(records$time))/(30-length(unique(records$issue)))),
                           round((7000-sum(records$calories))/(30-length(unique(records$issue)))))
        )
    })
    
    observe({
        f7Popover(
            targetId = "days",
            content = ifelse(as.numeric(today-firstday + 1) - length(unique(records$issue)) < 2 , 
                             "Everything is on track!", 
                             sprintf("You've missed %s times of workout!", 
                                     as.numeric(today-firstday + 1) - length(unique(records$issue)))),
            session
        )
    })
    
    observe({
        f7Popover(
            targetId = "time",
            content = ifelse(mean(records_by_day$time)<1000/30, 
                             sprintf("Averaged time (%s mins) is lower than target (%s mins).", round(mean(records$time)), round(1000/30)), 
                             "Keep the good work!"),
            session
        )
    })
    
    observe({
        f7Popover(
            targetId = "calories",
            content = ifelse(mean(records_by_day$calories)<7000/30, 
                             sprintf("Averaged calories (%s cals) is lower than target (%s cals).", round(mean(records$calories)), round(7000/30)), 
                             "Keep the good work!"),
            session
        )
    })
    
    output$Plot2 <- renderImage({
        list(src = calories$imgs[which(calories$food==input$food)],
             contentType = 'image/png',
             width = 300,
             height = 300)
    }, deleteFile = FALSE)
}

shinyApp(ui = ui, server = server)

