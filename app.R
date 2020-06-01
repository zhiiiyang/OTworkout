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
library(echarts4r.assets)
library(waiter)

source("analyzeWorkout.R")


ui = f7Page(iosTranslucentBars = TRUE,
            hideTabsOnPageScroll = TRUE,
            icon = "icons/icon-128x128.png",
            favicon = "icons/icon-512x512.png",
            manifest = "manifest.json",
            hideNavOnPageScroll = FALSE,
            hideTabsOnPageScroll = FALSE,
    f7TabLayout(
        use_waiter(),
        waiter_show_on_load(html = spin_heartbeat(),
                            color = "white"),
        
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
            #######
            # TAB 1
            #######
            
            f7Tab(
                tabName = "Today's task",
                icon = f7Icon("calendar_today"),
                active = TRUE,
                
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
                        f7Link(label = "Submit workout", src = sprintf("https://github.com/zhiiiyang/OTworkout/issues/%s", length(unique(records$issue))+1), external = TRUE),
                        f7Link(label = "Start workout", src = "https://apps.apple.com/us/app/orangetheory-fitness/id1424351827", external = TRUE)
                    )
                ),
                
                f7Card(
                    f7Swipeout(
                        tag = f7ListItem("Swipe right if you don't feel like doing it today!"),
                        side = "left",
                        f7SwipeoutItem(id = "suggestions", color = "pink", "For tomorrow"),
                        f7SwipeoutItem(id = "confirm", color = "green", "Let's do it!"),

                    )
                ),
                
            ), 
            
            #######
            # TAB 2
            #######   
            
            f7Tab(
                tabName = 'Videos',
                icon = f7Icon("videocam_fill"),
                
                f7Card(
                    title = "Bilibili",
                    "Here are all the inspirational vidoes from Bilibili.",
                    img = "https://esportsobserver.com/wp-content/uploads/2020/04/Bilibili-Investment.jpg",
                    footer = tagList(
                        f7Link(label = "1", src = "https://www.bilibili.com/video/BV1W7411c7rH", external = TRUE),
                        f7Link(label = "2", src = "https://www.bilibili.com/video/BV1XJ411x7yz", external = TRUE),
                        f7Link(label = "3", src = "https://www.bilibili.com/video/BV1YK4y1C7CU", external = TRUE),
                        f7Link(label = "4", src = "https://www.bilibili.com/video/BV1Nf4y1U7Av", external = TRUE),
                        f7Link(label = "5", src = "https://www.bilibili.com/video/BV18k4y167qH", external = TRUE),
                    )
                ),
                
                f7Card(
                    title = "Pamela Reif's YouTube channel",
                    "Here are Pamela Reif's workout videos.",
                    img = "https://i.ytimg.com/vi/Y2eOW7XYWxc/maxresdefault.jpg",
                    footer = tagList(
                        f7Link(label = "1", src = "https://www.youtube.com/watch?v=Q-vuR4PJh2c", external = TRUE),
                        f7Link(label = "2", src = "https://www.youtube.com/watch?v=RqfkrZA_ie0", external = TRUE),
                        f7Link(label = "3", src = "https://www.youtube.com/watch?v=UBMk30rjy0o", external = TRUE),
                        f7Link(label = "4", src = "https://www.youtube.com/watch?v=1f8yoFFdkcY", external = TRUE),
                        f7Link(label = "5", src = "https://www.youtube.com/watch?v=Fu_oExrPX68", external = TRUE),
                    )
                ),
                

                f7Card(
                    title = "Muscle Watching's YouTube channel",
                    "Here are Muscle Watching's workout videos.",
                    img = "https://i.pinimg.com/originals/30/ef/99/30ef99abd805a641345d55d4f3c9ad31.jpg",
                    footer = tagList(
                        f7Link(label = "1", src = "https://www.youtube.com/watch?v=fJaOFCCL1aU", external = TRUE),
                        f7Link(label = "2", src = "https://www.youtube.com/watch?v=hpJt16Rojqk", external = TRUE),
                        f7Link(label = "3", src = "https://www.youtube.com/watch?v=LV9_tRTqLyo", external = TRUE),
                        f7Link(label = "4", src = "https://www.youtube.com/watch?v=HODJFqMnQJA", external = TRUE),
                        f7Link(label = "5", src = "https://www.youtube.com/watch?v=60YEfkhmOOM", external = TRUE),
                    )
                ),
            ),
            
            #######
            # TAB 2
            #######            
            
            f7Tab(
                tabName = "Progress",
                icon = f7Icon("goforward_30"),
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
                            
                            f7PopoverTarget(f7Button(label = ifelse(mean(records_by_day$calories)<10500/30,"More calories!", "Calories OK!"), 
                                                     color = ifelse(mean(records_by_day$calories)<10500/30, "orange", "lightblue"),
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
                    paste0("Burned ", round(sum(records$calories)/70), "% of 10,500 calories (~ 3lb fat)"),
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
            
            #######
            # TAB 4
            #######
            f7Tab(
                tabName = "History",
                icon = f7Icon("book"),

                f7Card(
                    f7Swiper(
                       id = "point-swiper",
                       slidePerView = 1,
                       f7Slide(
                           echarts4rOutput("calories_plot",
                                           width = "400px", height = "280px") 
                       ),
                       
                       f7Slide(
                           echarts4rOutput("hr_plot",
                                           width = "400px", height = "280px") 
                       ),
                       
                       f7Slide(
                           echarts4rOutput("calories_calender",
                                           width = "400px", height = "280px") 
                       ),
                       
                       f7Slide(
                           echarts4rOutput("time_calender",
                                           width = "400px", height = "280px") 
                       ),                      
                   )
                ),
                
                f7Card(
                    f7DatePicker(
                        inputId = "date",
                        label = "Choose a date",
                        scrollToInput = TRUE, 
                        value = firstday + nrow(records_by_day) ,
                        minDate = firstday,
                        maxDate = firstday + nrow(records_by_day) + 1,
                        openIn = "customModal",
                        direction = "vertical"
                    ),
                    
                    uiOutput("timeline"),
                    
                    f7Swiper(
                        id = "gauge-swiper",
                        slidePerView = 1,
                        center = TRUE,
                        f7Slide(
                            echarts4rOutput("gauge_plot_hr",
                                            height = "300px")
                            
                        ),
                        f7Slide(
                            echarts4rOutput("gauge_plot_maxhr",
                                            height = "300px")
                        ) 
                    )
                )        
            ),
            
            #######
            # TAB 4
            #######
            f7Tab(
                tabName = "Calculator",
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
    Sys.sleep(3) # do something that takes time
    waiter_hide()
    
    # TAB1
    output$frequency <- renderText({
        freq <- sum(as.numeric(records$calories))/calories$calories[which(calories$food==input$food)]
        paste0("After ", nrow(records), " workouts, you have burned calories same as ", round(freq, 1), " units.")
    })
    

    observeEvent(input$suggestions, {
        f7Notif(
            title = "For the next workout",
            icon = f7Icon("hand_point_right"),
            text = sprintf("You need to work out for > %s mins and burn > %s calories",
                           round((1000-sum(records$time))/(30-length(unique(records$issue)))),
                           round((7000-sum(records$calories))/(30-length(unique(records$issue))))),
            session = session

        )
    })

    observeEvent(input$confirm, {
        f7Dialog(
            inputId = "prompt",
            title = "Hey!",
            text = "Will you work out today?",
            type = "prompt",
            session = session
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
            content = ifelse(mean(records_by_day$calories)<10500/30, 
                             sprintf("Averaged calories (%s cals) is lower than target (%s cals).", round(mean(records$calories)), round(10500/30)), 
                             "Keep the good work!"),
            session
        )
    })
    
    # TAB2
    output$calories_plot <- renderEcharts4r({
        records_by_day$time <- round(records_by_day$time)
        records_by_day %>%
            e_chart(date) %>%
            e_effect_scatter(calories, time, symbol = "pin", name = "Calories") %>%
            e_scatter(time, symbol = ea_icons("clock"), symbol_size = 20, y_index = 1, name = "Time") %>%
            e_mark_line(data = list(yAxis = round((10500-sum(records_by_day$calories))/(30-nrow(records_by_day)))), title = "Target") %>%
            e_tooltip(trigger = "axis") %>%
            e_y_axis(min = 0) 
        
    })
        
    output$hr_plot <- renderEcharts4r({
        records_by_day %>%
            e_chart(date) %>%
            e_effect_scatter(hr, name = "Averaged heart rate", symbol = ea_icons("heart")) %>%
            e_effect_scatter(maxhr, name = "Maximum heart rate", symbol = ea_icons("heart")) %>%
            e_mark_line(data = list(yAxis = round(189*0.84)), title = "Orange") %>%
            e_mark_line(data = list(yAxis = round(189*0.71)), title = "Challenge") %>%
            e_color(
                c("#ea710d", "#b00204")
            ) %>%
            e_legend(type = "scroll") %>%
            e_tooltip(trigger = "axis") %>%
            e_y_axis(min = 60) 
    })
    
    output$calories_calender <- renderEcharts4r({
        records_by_day %>% 
            mutate(date = as.Date(records_by_day$date, format = "%m/%d")) %>%
            e_charts(date) %>% 
            e_calendar(range = c("2020-05", "2020-07")) %>% 
            e_heatmap(calories, coord_system = "calendar") %>% 
            e_visual_map(min = 100, max = 800, right = 10, bottom = 50) %>% 
            e_tooltip() %>%
            e_title("Daily calories")
    })
    
    output$time_calender <- renderEcharts4r({
        records_by_day %>% 
            mutate(date = as.Date(records_by_day$date, format = "%m/%d")) %>%
            e_charts(date) %>% 
            e_calendar(range = c("2020-05", "2020-07")) %>% 
            e_heatmap(time, coord_system = "calendar") %>% 
            e_visual_map(min = 20, max = 100, right = 10, bottom = 50) %>% 
            e_tooltip() %>%
            e_title("Daily workout time")        
    })
    
    index <- reactive({
        which(records$issue==as.numeric(as.Date(input$date)-firstday +2))
    })
    
    output$timeline <- renderUI({
        timeline <- lapply(index(), function(i){
            f7TimelineItem("",
                           date = records$date[i],
                           card = TRUE,
                           time = records$uploadtime[i],
                           title = sprintf("Time: %s min", round(records$time[i])),
                           subtitle = sprintf("%s calories", round(records$calories[i])),
                           side = "right")
        }) %>% do.call(tagList, .)
        
        f7Timeline(timeline,
                   sides = TRUE)
    })
    
    output$gauge_plot_hr <- renderEcharts4r({
        e_charts() %>% 
            e_gauge(round(max(records$hr[index()])/189*100, 1), "PERCENT") %>% 
            e_title("Average heart rate") 
    })
    
    output$gauge_plot_maxhr <- renderEcharts4r({
        e_charts() %>% 
            e_gauge(round(max(records$maxhr[index()])/189*100, 1), "PERCENT") %>% 
            e_title("Maximum heart rate") 
    })  

    # TAB 4
    output$Plot2 <- renderImage({
        list(src = calories$imgs[which(calories$food==input$food)],
             contentType = 'image/png',
             width = 300,
             height = 300)
    }, deleteFile = FALSE)
}

shinyApp(ui = ui, server = server)

