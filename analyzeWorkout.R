target_cal <- 3500*2.25

#######################
# process close issues 
#######################
firstday <- as.Date("2020-05-19")


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

records <- c()
for (comment in comments){
  fig <- str_extract(comment$body, url_pattern)
  fig <- gsub(")","",fig)
  
  issue <- as.numeric(gsub("https://api.github.com/repos/zhiiiyang/OTworkout/issues/", "", comment$issue_url))
  date <- format(firstday + issue - 2, "%m/%d")
  
  uploadtime <- sprintf("%02s:%02s",
                        hour(with_tz(ymd_hms(comment$created_at), "America/Los_Angeles")),
                        minute(with_tz(ymd_hms(comment$created_at), "America/Los_Angeles")))
  
  cal_points <- image_read(fig) %>%
    image_crop('828x110+0+680') %>%
    tesseract::ocr() %>%
    str_extract_all("[0-9]+") %>%
    first(default = NULL) %>%
    as.numeric()
  
  if(length(cal_points)==2){
    c(calories, spalshpoints) %<-% cal_points
  } else {
    c(calories, spalshpoints) %<-% c(0, 0)
  }
  
  hour <- image_read(fig) %>%
    image_crop('340x80+0+160') %>%
    tesseract::ocr() %>%
    gsub("[\r\n]", "", .) %>%
    str_extract_all("([0-9]+)h") %>% 
    first(default = NULL) %>%
    gsub("h","", .) %>%
    as.numeric()
  
  min <- image_read(fig) %>%
    image_crop('340x80+0+160') %>%
    tesseract::ocr() %>%
    gsub("[\r\n]", "", .) %>%
    str_extract_all("([0-9]+)m") %>% 
    first(default = NULL) %>%
    gsub("m","", .) %>%
    as.numeric()
  
  if(length(hour)>0){
    time <- hour*60 + min
  } else {
    time <- min
  }
  
  # heart rates 
  image_read(fig) %>%
    image_crop('828x80+0+1030') %>%
    image_ocr() %>%
    str_extract_all("([0-9]+)") %>% 
    first(default = NULL) %>%
    as.numeric() -> heartrates
  
  if(length(heartrates)==2){
    c(hr, maxhr) %<-% heartrates
  } else {
    c(hr, maxhr) %<-% c(0, 0)
  }
  
  records <- rbind(records,
                       data.frame(date, issue, uploadtime, 
                                  time, calories = as.numeric(calories), 
                                  spalshpoints = as.numeric(spalshpoints), hr =as.numeric(hr), 
                                  maxhr = as.numeric(maxhr), 
                                  stringsAsFactors = FALSE) )
}

records_by_day <- records %>% group_by(date) %>%
  summarize(Freq = n(), 
            Calories = sum(calories),
            Time = sum(time),
            hr = max(hr), 
            maxhr = max(maxhr)) %>%
  rename(Date = date) %>%
  mutate(cal_prev = percent((Calories - lag(Calories, default = 0))/Calories, digits = 0)) 



########################
# TAB 1: liquid
########################

liquid <- data.frame(value = c(length(issues_closed)/30, sum(records$time)/1000, sum(records$calories)/7000),
                     color = c("darkturquoise", "limegreen", "crimson"),
                     legend = c("30 workouts", "1,000 mins", "7,000 calories (~ 2lb fat)"))

liquid <- liquid[order(liquid$value, decreasing = TRUE),]


####################
# TAB 3: ring plot
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
png("www/ring.png",width = 200*30, height = 140*30, 
    res = 72*100)
p <- ggplot(data = data) +
  geom_rect(aes(ymax=ymax-0.002, ymin=ymin+0.002, xmax=3, xmin=2, fill=category)) +
  geom_text( x=0, y = 0, 
             label = paste0("Day ", nrow(records_by_day)), 
             size=1) + 
  scale_fill_manual(values = c(hcl.colors(30, palette = "Temp", rev = TRUE)[1:nrow(records_by_day)],
                               rep("gray95", 30 - nrow(records_by_day))))+
  coord_polar(theta="y") +
  xlim(c(0, 3)) +
  theme_void() +            
  theme(legend.position = "none",
        plot.margin = unit(c(-0.05,0,-0.05,0), "inches"))
print(p)
dev.off()


####################
# TAB 3: buttoms
####################
today <- as_date(with_tz(Sys.time(), "America/Los_Angeles"))
ontrack <- ifelse(as.numeric(today-firstday+1) == nrow(records), 
                  "on track", 
                  paste("miss", as.numeric(today-firstday+1) - nrow(records), "days"))
todaystatus <- ifelse(as.numeric(today-firstday + 1) == nrow(records),
                      "yes",
                      "no")

buttom_df <- data.frame(value = c(as.numeric(today-firstday + 1) - length(unique(records$issue)) < 2,
                                  mean(records_by_day$Time)<1000/30, 
                                  mean(records_by_day$Calories)<7000/30))
buttom_df$color <- sapply(buttom_df$value, function(x) ifelse(x==TRUE,"orange", "lightblue"))

# time 
buttom_df$label[1] 


########################
# TAB 5
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



# icons 
## https://icons8.com/icons/set/30-days
