#----------------------
# Setting up Packages -
#----------------------

# Installing necessary packages
library("httr")
library("data.table")
library("tictoc")

library("readr")
library("xml2")

# function to implement create a wait time to not overload the BGG API
wait <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1
}

beefsack <- "https://raw.githubusercontent.com/beefsack/bgg-ranking-historicals/master/" # Beefsack's github URL
beefsack_date <- as.character(Sys.Date()) # Date to be passed to the 
beefsack_api <- paste0(beefsack, beefsack_date, ".csv")

beefsack_data <- data.table(read_csv(beefsack_api))
id_list <- beefsack_data[, ID]
rm(beefsack, beefsack_api, beefsack_date, beefsack_data)


bgg1 <- "https://www.boardgamegeek.com/xmlapi2/thing?id="
bgg2.1 <- "&ratingcomments=1&page=1"
reviews.dt = data.table()


i = 2
for( i in 1:5) {
  active_id <- id_list[i]
  bgg_api <- paste0(bgg1, active_id, bgg2.1)

  bgg_data <- read_xml(bgg_api)
  comments <- xml_find_all(bgg_data, "//comments")
  number_of_ratings <- as.integer(xml_attr(comments, "totalitems"))
  total_pages <- ceiling(number_of_ratings/50)
  n = 1

  tic("getting reviews")
  for( n in 1:total_pages) {
    bgg_data <- read_xml(bgg_api)
    temp <- data.table(bgg_data$items)
    page_reviews.dt <- temp[, c("collid", "objectid", "user.username", "status.own", "status.prevowned", "status.fortrade", "status.wanttoplay", "textfield.comment.value",)]

    setnames(page_reviews.dt, c("Review ID", "Game ID", "Username", "Rating", "Comment"))
    rm(username, rating, review)
  
    reviews.dt <- rbind(reviews.dt, page_reviews.dt)
    
    page = n + 1
    bgg2.2 = paste0("&ratingcomments=1&page=", page)
    bgg_api <- paste0(bgg1, active_id, bgg2.2)
    wait(2)
  }
  toc()
  
i = i + 1
}

setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Reviews")
fwrite(reviews.dt, "reviews_top_5.csv")
