#----------------------
# Setting up Packages -
#----------------------

# Installing necessary packages
library("httr")
library("data.table")
library("tictoc")
library("readr")
library("xml2")
library("jsonlite")

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

api.game <- "https://api.geekdo.com/api/collections?ajax=1&objectid="
api.page <- "&objecttype=thing&oneperuser=1&pageid="
api.end <- "&require_review=true&showcount=50&sort=rating_tstamp"

api.game1 <- "https://www.boardgamegeek.com/xmlapi2/thing?id="
api.pages <- "&ratingcomments=1&page=1"
reviews.dt = data.table()

#----------------------------------------
# The setup part 2: Finding New Reviews -
#----------------------------------------

setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Reviews")
Existing_Reviews_Counts.dt <- fread("current_review_counts.csv")
Reviews_Second.dt <- fread("New Reviews.csv")

i = 1
for( i in 16:30) {
  active_id <- id_list[i]
  bgg_api <- paste0(api.game1, active_id, api.pages)
  
  bgg_data <- read_xml(bgg_api)
  comments <- xml_find_all(bgg_data, "//comments")
  number_of_ratings <- as.integer(xml_attr(comments, "totalitems"))
  if(active_id %in% Existing_Reviews_Counts.dt$ID ) {
    new_ratings <-number_of_ratings - Existing_Reviews_Counts.dt[ID == active_id, `Users rated`]
    last_review_date <- as.Date("2019-05-02")
  } else {
    new_ratings <- number_of_ratings
    last_review_date <- as.Date("01/01/2000", format = "%m/%d/%y")
  }
  total_pages <- ceiling(new_ratings/50)
  page = 1
  api.2 <- paste0(api.game, active_id, api.page, page , api.end)
  min_date <- Sys.Date()
  
  tic("getting reviews")
  while( min_date >= last_review_date ) {
    bgg_data <- fromJSON(api.2, flatten = TRUE)
    temp <- data.table(bgg_data$items)
    page_reviews.dt <- temp[, c("collid", "objectid", "user.username", "textfield.comment.value", "textfield.comment.tstamp", "rating", "rating_tstamp")]
                               # "status.own", "status.prevowned", "status.fortrade", "status.wanttoplay", # Removed for now. May try to include later. These appear based on use.
    setnames(page_reviews.dt, c("Review ID", "Game ID", "Username", "Comment", "Comment Date", "Rating", "Rating Date"))
                               # "Own", "Previously Owned", "For Trade", "Want to Play", # See Above comment
    rm(temp)
    reviews.dt <- rbind(reviews.dt, page_reviews.dt)
    
    page = page + 1
    api.2 <- paste0(api.game, active_id, api.page, page , api.end)
    min_date <- page_reviews.dt[, .(min(as.Date(`Rating Date`)))][[1]]
    wait(3.5)
  }
  toc()
  
  i = i + 1
}
reviews.dt <- unique(reviews.dt)


setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Reviews")

fwrite(reviews.dt, "reviews_top_6.csv")
