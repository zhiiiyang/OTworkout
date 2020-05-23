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



