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
library(formattable)
library(sparkline)
library(httr)
library(purrr)
library(DT)

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
                    authOr = "Zhi",
                    date = as.Date(with_tz(Sys.Date(), "America/Los_Angeles")),
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
                    title = "Workout",
                    "Here are all your favourite videos.",
                    img = "https://esportsobserver.com/wp-content/uploads/2020/04/Bilibili-Investment.jpg",
                    footer = tagList(
                        f7Link(label = "Arms", src = "https://www.bilibili.com/video/BV1W7411c7rH", external = TRUE),
                        f7Link(label = "Body", src = "https://www.bilibili.com/video/BV1XJ411x7yz", external = TRUE),
                        f7Link(label = "Legs", src = "https://www.bilibili.com/video/BV1yb411G7Me", external = TRUE),

                    )
                ),
                
                f7Card(
                    title = "Pamela Reif's YouTube channel",
                    "Here are Pamela Reif's workout videos.",
                    img = "https://i.ytimg.com/vi/Y2eOW7XYWxc/maxresdefault.jpg",
                    footer = tagList(
                        f7Link(label = "Full-body", src = "https://www.youtube.com/watch?v=Cw-Wt4xKD2s", external = TRUE),
                        f7Link(label = "Abs", src = "https://www.youtube.com/watch?v=1f8yoFFdkcY", external = TRUE),
                        f7Link(label = "Leg", src = "https://www.youtube.com/watch?v=Fu_oExrPX68", external = TRUE),
                    )
                ),

                f7Card(
                    title = "Muscle Watching's YouTube channel",
                    "Here are Muscle Watching's workout videos.",
                    img = "https://i.pinimg.com/originals/30/ef/99/30ef99abd805a641345d55d4f3c9ad31.jpg",
                    footer = tagList(
                        f7Link(label = "Legs", src = "https://www.youtube.com/watch?v=fJaOFCCL1aU", external = TRUE),
                        f7Link(label = "Stretch", src = "https://www.youtube.com/watch?v=hpJt16Rojqk", external = TRUE),
                        f7Link(label = "Full-body", src = "https://www.youtube.com/watch?v=60YEfkhmOOM", external = TRUE),
                    )
                ),
            ),
            
            #######
            # TAB 3
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
                        date = as.Date(with_tz(Sys.Date(), "America/Los_Angeles")),
                        div(img(src = "ring.png", width = "100%"), style="text-align: center;"),
                        footer = tagList(
                            f7PopoverTarget(f7Button(label = paste(30 - nrow(records_by_day[-c(1:30), ]), "days left"), 
                                                     color = ifelse(as.numeric(today-firstday + 1) - length(unique(records$issue)) < 2, "lightblue", "orange"),
                                                     rounded = TRUE),
                                            targetId = "days"),    
                            
                                f7PopoverTarget(f7Button(label = ifelse(mean(records_by_day$Time)<target_time/30, "Longer time!", "Time OK!"), 
                                                    color = ifelse(mean(records_by_day$Time)<target_time/30, "orange", "lightblue"),
                                                    rounded = TRUE),
                                            targetId = "time"),
                            
                            f7PopoverTarget(f7Button(label = ifelse(mean(records_by_day$Calories)<target_cal/30,"More calories!", "Calories OK!"), 
                                                     color = ifelse(mean(records_by_day$Calories)<target_cal/30, "orange", "lightblue"),
                                                     rounded = TRUE),
                                            targetId = "calories"),
                        )
                    ) 
                ),
                f7Card(
                    paste0("Completed ", round(nrow(records_by_day[-c(1:30), ])/30*100), "% of 30 workouts"),
                    f7Progress(id = "p1", value = nrow(records_by_day[-c(1:30), ])/30*100, color = "green")
                ),
                f7Card(
                    paste0("Spent ", round(sum(records_by_day[-c(1:30), "Time"])/target_time*100), sprintf("%% of %s hrs", round(target_time/60, 1))),
                    f7Progress(id = "p2", value = sum(records_by_day[-c(1:30), "Time"])/target_time*100, color = "lightblue"),
                ),
                f7Card(
                    paste0("Burned ", round(sum(records_by_day[-c(1:30), "Calories"])/target_cal*100), sprintf("%% of %sk calories (~ 3lb fat)", round(target_cal/1000, 1))),
                    f7Progress(id = "p3", value = sum(records_by_day[-c(1:30), "Calories"])/target_cal*100, color = "red")
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
                    formattableOutput("table1")    
                ),           
            
                
                f7Card(
                    f7Swiper(
                       id = "point-swiper",
                       slidePerView = 1,
                       f7Slide(
                           dataTableOutput("table2")
                       ), 
                       
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
                        maxDate = firstday + nrow(records_by_day),
                        openIn = "customModal",
                        direction = "vertical"
                    ),
                    uiOutput("timeline")
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
    Sys.sleep(1) # do something that takes time
    waiter_hide()
    
    # TAB1
    output$frequency <- renderText({
        freq <- sum(as.numeric(records$calories))/calories$calories[which(calories$food==input$food)]
        paste0("After ", length(issues_closed), " workouts, you have burned calories same as ", round(freq, 1), " units.")
    })
    

    observeEvent(input$suggestions, {
        f7Notif(
            title = "For the next workout",
            icon = f7Icon("hand_point_right"),
            text = sprintf("You need to work out for > %s mins and burn > %s calories",
                           round((target_time-sum(records$time))/(30-length(unique(records$issue)))),
                           round((target_cal-sum(records$calories))/(30-length(unique(records$issue))))),
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
                                     30 - length(issues_closed))),
            session
        )
    })
    
    observe({
        f7Popover(
            targetId = "time",
            content = ifelse(mean(records_by_day$Time)<target_time/30, 
                             sprintf("Averaged time (%s mins) is lower than target (%s mins).", round(mean(records$time)), round(target_time/30)), 
                             "Keep the good work!"),
            session
        )
    })
    
    observe({
        f7Popover(
            targetId = "calories",
            content = ifelse(mean(records_by_day$Calories)<target_cal/30, 
                             sprintf("Averaged calories (%s cals) is lower than target (%s cals).", round(mean(records$calories)), round(target_cal/30)), 
                             "Keep the good work!"),
            session
        )
    })
    
    # TAB2
    output$calories_plot <- renderEcharts4r({
        records_by_day$Time <- round(records_by_day$Time)
        records_by_day %>%
            e_chart(Date) %>%
            e_effect_scatter(Calories, Time, symbol = "pin", name = "Calories") %>%
            #e_scatter(Time, symbol = ea_icons("clock"), symbol_size = 20, y_index = 1, name = "Time") %>%
            e_line(Calories) %>%
            e_line(Time, y_index = 1) %>%
            e_mark_line(data = list(yAxis = round((target_cal-sum(records_by_day$Calories))/(30-length(issues_closed)))), title = "Target") %>%
            e_tooltip(trigger = "axis") %>%
            e_y_axis(min = 0)
        
    })
        
    output$hr_plot <- renderEcharts4r({
        records_by_day %>%
            e_chart(Date) %>%
            # e_effect_scatter(hr, name = "Averaged heart rate", symbol = ea_icons("heart")) %>%
            # e_effect_scatter(maxhr, name = "Maximum heart rate", symbol = ea_icons("heart")) %>%
            e_step(hr) %>%
            e_step(maxhr) %>%
            e_mark_line(data = list(yAxis = round(189*0.84)), title = "Orange") %>%
            e_mark_line(data = list(yAxis = round(189*0.71)), title = "Challenge") %>%
            e_color(
                c("#ea710d", "#b00204")
            ) %>%
            e_tooltip(trigger = "axis") %>%
            e_y_axis(min = 60) 
    })
    
    output$calories_calender <- renderEcharts4r({
        records_by_day %>% 
            mutate(Date = as.Date(records_by_day$Date, format = "%m/%d")) %>%
            e_charts(Date) %>% 
            e_calendar(range = c("2020-05", "2020-07")) %>% 
            e_heatmap(Calories, coord_system = "calendar") %>% 
            e_visual_map(min = 100, max = 800, right = 10, bottom = 50,
                         inRange = list(color = c("#f6f6f6", "#339676"))) %>% 
            e_tooltip() %>%
            e_title("Daily calories")
    })
    
    output$time_calender <- renderEcharts4r({
        records_by_day %>% 
            mutate(Date = as.Date(records_by_day$Date, format = "%m/%d")) %>%
            e_charts(Date) %>% 
            e_calendar(range = c("2020-05", "2020-07")) %>% 
            e_heatmap(Time, coord_system = "calendar") %>% 
            e_visual_map(min = 20, max = 100, right = 10, bottom = 50, 
                         inRange = list(color = c("#f7f7f7", "red"))) %>% 
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
    
    # https://getbootstrap.com/docs/3.3/components/
    output$table1 <- renderFormattable({
        df_table <- records_by_day[nrow(records_by_day):(nrow(records_by_day)-6), ]

        df_table$hr <- cut(df_table$hr, 
                                      breaks = c(-Inf, 189*0.6, 189*0.7, 189*0.84, 189*0.91, Inf),
                                      labels = c("Light", "Warm-up", "Challenging",
                                                 "Orange effect","All out effort")) %>% as.character()
        
        df_table$maxhr <- cut(df_table$maxhr, 
                                         breaks = c(-Inf, 189*0.6, 189*0.7, 189*0.84, 189*0.91, Inf),
                                         labels = c("Light", "Warm-up", "Challenging",
                                                    "Orange effect","All out effort")) %>% as.character()
        colnames(df_table)[c(5:7)]<-c("HR", "MaxHR", "ΔCal")
        df_table <- df_table[, c(1:3, 7, 4:6)]
        
        formattable(df_table, list(
            Freq = formatter("span", style = x ~ ifelse(x > 1, 
                                                        style(color = "green", font.weight = "bold"), NA)),
            area(col = c(Calories)) ~ normalize_bar("pink", 0.2),
            Time = formatter("span",
                             style = x ~ style(color = ifelse(x>target_time/30, "green", "red")),
                             x ~ icontext(ifelse(x>target_time/30, "ok", "remove"))),
            HR = formatter("span",
                           style = x ~ style(color = ifelse(x=="Light", "#3d84c9", ifelse(x=="Warm-up", "#339676", ifelse(x=="Challenging", "#f97108", "#af0500")))),
                           x ~ icontext(ifelse(x==1, "heart", "heart"))),
            `MaxHR` = formatter("span",
                                 style = x ~ style(color = ifelse(x=="Light", "#3d84c9", ifelse(x=="Warm-up", "#339676", ifelse(x=="Challenging", "#f97108", "#af0500")))),
                                 x ~ icontext(ifelse(x==1, "heart", "heart"))),
             `ΔCal` = formatter(
                "span",
                style = x ~ style(color = ifelse(x < 0 , "red", "green")),
                x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x))
        ))
    })
    
    
    output$table2 <- renderDataTable({
        df_table2 <- records_by_day
        df_table2$Day <- lubridate::wday(as.Date(df_table2$Date, "%m/%d"), label = TRUE)
        df_table2$Week <- (1:nrow(df_table2)) %/% 7 +1

        
        my_sparkline_bar <- function(x) {
            sparkline(x, type = "bar",
                      barColor = "#B00204",
                      negBarColor = "#EA710D"
            )
        }
        
        my_sparkline_bar2 <- function(x) {
            sparkline(x, type = "bar",    
                      barColor = "#339676",
                      negBarColor = "#3d84c9"
            )
        }
        
        my_sparkline_line <- function(x) {
            sparkline(x, type = "line", fillColor = FALSE,  
                      normalRangeMax = target_cal/30, normalRangeMin = 0, 
                      normalRangeColor = "#deebf7", lineColor = "black", minSpotColor = FALSE, spotColor = FALSE)
        }
        
        my_sparkline_line2 <- function(x) {
            sparkline(x, type = "line", fillColor = FALSE,  
                      normalRangeMax = target_time/30, normalRangeMin = 0, 
                      normalRangeColor = "#deebf7", lineColor = "black", minSpotColor = FALSE, spotColor = FALSE)
        }
        
        
        df <- df_table2 %>% group_by(Week) %>%
            summarise()
        
        df$Calories <- (df_table2$Calories) %>%
            split(df_table2$Week) %>%
            map(my_sparkline_line) %>%
            map(htmltools::as.tags) %>%
            map_chr(as.character)
        
        df$Time <- (df_table2$Time) %>%
            split(df_table2$Week) %>%
            map(my_sparkline_line2) %>%
            map(htmltools::as.tags) %>%
            map_chr(as.character)
        
        df$`MaxHR` <- (df_table2$maxhr - 189*0.84) %>%
            split(df_table2$Week) %>%
            map(my_sparkline_bar) %>%
            map(htmltools::as.tags) %>%
            map_chr(as.character)
        
        df$`HR`<- (df_table2$hr - 189*0.71) %>%
            split(df_table2$Week) %>%
            map(my_sparkline_bar2) %>%
            map(htmltools::as.tags) %>%
            map_chr(as.character)
        
        draw_callback <- htmlwidgets::JS("function(){HTMLWidgets.staticRender()}")
        dt_options <- list(fnDrawCallback = draw_callback,
                           columnDefs = list(list(
                               className = 'dt-left', targets = 1:4,
                               className = 'dt-bottom', targets = 1:4
                           )),
                           searching = FALSE,
                           paging = FALSE, 
                           info = FALSE,
                           headerCallback = DT::JS(
                               "function(thead) {",
                               "  $(thead).css('font-size', '0.4em');",
                               "}"
                           ))
        datatable(df, options = dt_options, escape = FALSE, 
                  rownames = FALSE) %>%
            formatStyle(columns = 1, fontSize = "40%") %>%
            spk_add_deps()

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

