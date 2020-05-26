library(tesseract)
library(stringr)
library(zeallot)
library(gh)
library(ggplot2)
library(lubridate)
library(shinyMobile)
library(shiny)

#######################
# process close issues 
#######################
issues_closed <- gh(
  "GET /repos/:owner/:repo/issues",
  owner = "zhiiiyang",
  repo = "OTworkout",
  state = "closed",
  .token = Sys.getenv("GITHUB_PAT", "")
)

comments <- gh(
  "GET /repos/:owner/:repo/issues/comments",
  owner = "zhiiiyang",
  repo = "OTworkout",
  .token = Sys.getenv("GITHUB_PAT", "")
)


url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

record_list <- lapply(comments, function(comment) {
  fig <- str_extract(comment$body, url_pattern)
  fig <- gsub(")","",fig)
  eng <- tesseract("eng")
  text <- tesseract::ocr(fig, engine = eng) 
  strings <- str_split(text, "\n")[[1]]
  
  date <- unlist(strsplit(strings[grep("2020", strings)], ", "))[2]
  uploadtime <- sprintf("%s:%s",
                        hour(with_tz(ymd_hms(comment$created_at), "America/Los_Angeles")),
                        minute(with_tz(ymd_hms(comment$created_at), "America/Los_Angeles")))
  
  # return(calories_points)
  c(calories, spalshpoints) %<-% str_split(strings[9], " ")[[1]]

  c(min, second) %<-%  str_extract_all(strings[grep("2020", strings)+1], "(\\d)+")[[1]] 
  time <- as.numeric(min) + as.numeric(second)/60

  heartrates <- strings[grep("AVERAGE HEART RATE", strings)-1]
  c(hr, maxhr) %<-% str_split(heartrates, " ")[[1]]

  distance <- strings[grep("MILES STEPS", strings)-1]
  c(miles, steps) %<-% str_split(distance, " ")[[1]]

  return(data.frame(date, uploadtime, 
                    time, calories = as.numeric(calories), 
                    spalshpoints = as.numeric(spalshpoints), hr =as.numeric(hr), 
                    maxhr = as.numeric(maxhr), miles, steps,
                    stringsAsFactors = FALSE) )
}) 

records <- do.call(rbind, record_list)

####################
# generate timeline
####################

timeline <- lapply(nrow(records):1, function(i){
  f7TimelineItem("",
                 date = records$date[i],
                 card = TRUE,
                 time = records$uploadtime[i],
                 title = sprintf("Time: %s min", round(records$time[i])),
                 subtitle = sprintf("%s calories", round(records$calories[i])),
                 side = "right")
}) %>% do.call(tagList, .)

####################
# generate ring plot 
####################
# Create test data.
data <- data.frame(
  category=paste("Day", 1:30),
  count=rep(1, 30)
)

# Compute percentages
data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)

# Make the plot
data$category <- factor(data$category, levels = paste("Day", 1:30))

# A temp file to save the output.
# This file will be removed later by renderImage
outfile <- file.create("www/ring.png")

# Generate the PNG
png("www/ring.png",width = 200*5, height = 150*5, 
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
        plot.margin = unit(c(-0.05,0,-0.05,0), "inches"))
print(p)
dev.off()


########################
# generate ring plot 2
########################

liquid <- data.frame(value = c(length(issues_closed)/30, sum(records$time)/1000, sum(records$calories)/7000),
                     color = c("darkturquoise", "limegreen", "crimson"),
                     legend = c("30 workouts", "1,000 mins", "7,000 calories (~ 2lb fat)"))


########################
# generate food units 
########################
# calories 
calories <- data.frame(food = c("Boba tea", "Rice",
                                "Instant noodles", "Chips",
                                "Daily calories burn"),
                       calories = c(550, 200, 470, 1200, 1600),
                       imgs = c("www/Boba.png",
                                "www/Rice.png",
                                "www/Instant.png",
                                "www/Chips.png",
                                "www/Daily.png"),
                       stringsAsFactors = FALSE)

# day

firstday <- as.Date("2020-05-19")
today <- as_date(with_tz(Sys.time(), "America/Los_Angeles"))
ontrack <- ifelse(as.numeric(today-firstday+1) == nrow(records), 
                  "on track", 
                  paste("miss", as.numeric(today-firstday+1) - nrow(records), "days"))
todaystatus <- ifelse(as.numeric(today-firstday + 1) == nrow(records),
                      "yes",
                      "no")

# icons 
## https://icons8.com/icons/set/30-days
