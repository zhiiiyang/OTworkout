library(tesseract)
library(stringr)
library(zeallot)
library(gh)
library(ggplot2)

# download close issues 
issues_closed <- gh(
  "GET /repos/:owner/:repo/issues",
  owner = "zhiiiyang",
  repo = "OTworkout",
  state = "closed"
)

comments <- gh(
  "GET /repos/:owner/:repo/issues/comments",
  owner = "zhiiiyang",
  repo = "OTworkout"
)


url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

record_list <- lapply(comments, function(comment) {
  fig <- str_extract(comment$body, url_pattern)
  fig <- gsub(")","",fig)
  eng <- tesseract("eng")
  text <- tesseract::ocr(fig, engine = eng) 
  strings <- str_split(text, "\n")[[1]]
  
  date <- strings[grep("2020", strings)]
  
  # return(calories_points)
  c(calories, spalshpoints) %<-% str_split(strings[9], " ")[[1]]

  c(min, second) %<-%  str_extract_all(strings[grep("2020", strings)+1], "(\\d)+")[[1]] 
  time <- as.numeric(min) + as.numeric(second)/60

  heartrates <- strings[grep("AVERAGE HEART RATE", strings)-1]
  c(hr, maxhr) %<-% str_split(heartrates, " ")[[1]]

  distance <- strings[grep("MILES STEPS", strings)-1]
  c(miles, steps) %<-% str_split(distance, " ")[[1]]

  return(data.frame(date, time, calories, spalshpoints, hr, maxhr, miles, steps,
                    stringsAsFactors = FALSE) )
}) 

records <- do.call(rbind, record_list)
records[, c(2,3,4,5,6)] <- apply(records[, c(2,3,4,5,6)], 2, as.numeric)

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


# calories 
calories <- data.frame(food = c("Boba tea", "Rice",
                                "Instant noodles", "Chips",
                                "Daily calories burn"),
                       calories = c(550, 200, 470, 1200, 1600),
                       imgs = c("imgs/Boba.png",
                                "imgs/Rice.png",
                                "imgs/Instant.png",
                                "imgs/Chips.png",
                                "imgs/Daily.png"),
                       stringsAsFactors = FALSE)

# day

firstday <- as.Date("2020-05-20")
ontrack <- ifelse(as.numeric(Sys.Date()-firstday+1) == nrow(records), 
                  "on track", 
                  paste("miss", as.numeric(Sys.Date()-firstday+1) - nrow(records), "days"))


# icons 
## https://icons8.com/icons/set/30-days
