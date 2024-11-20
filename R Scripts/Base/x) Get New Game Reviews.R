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

#-----------------------
# Getting Today's Data -
#-----------------------

beefsack <- "https://raw.githubusercontent.com/beefsack/bgg-ranking-historicals/master/" # Beefsack's github URL
beefsack_date <- as.character(Sys.Date()) # Date to be passed to the 
beefsack_api <- paste0(beefsack, beefsack_date, ".csv")

beefsack_data <- data.table(read_csv(beefsack_api))
id_list <- beefsack_data[, ID]

Current_Reviews_Count.dt <- beefsack_data[, c("ID", "Users rated")]
rm(beefsack, beefsack_api, beefsack_date, beefsack_data)

#---------------------------
# Reading in Historic Data -
#---------------------------

setwd("C:/Users/v-pemats/OneDrive - Calligo Limited/Onboarding/Board Game Geek Dashboard/Raw Data")
Reviews.dt <- fread("bgg-13m-reviews.csv")

Existing_Reviews_Counts.dt <- Reviews.dt[, .(`Users rated` =.N), by = ID]

#---------------------------
# Products With No Reviews -
#---------------------------

New_Ids <- Current_Reviews_Count.dt[!Existing_Reviews_Counts.dt, on = .(ID = ID)]
id_list <- New_Ids[,ID]


bgg1 <- "https://www.boardgamegeek.com/xmlapi2/thing?id="
bgg2.1 <- "&ratingcomments=1&page=1"
reviews.dt = data.table()

i = 1
tic("total batch time")
for( i in 1000:1707) {
  active_id <- id_list[i]
  bgg_api <- paste0(bgg1, active_id, bgg2.1)
  
  bgg_data <- read_xml(bgg_api)
  comments <- xml_find_all(bgg_data, "//comments")
  number_of_ratings <- as.integer(xml_attr(comments, "totalitems"))
  total_pages <- ceiling(number_of_ratings/100)
  n = 1
  
  tic("getting reviews")
  for( n in 1:total_pages) {
    bgg_data <- read_xml(bgg_api)
    
    comment <- xml_find_all(bgg_data, "//comments/comment")
    username <- xml_attr(comment, "username")
    rating <- xml_attr(comment, "rating")
    review <- xml_attr(comment, "value")
    
    page_reviews.dt <- data.table(cbind(active_id, username, rating, review))
    setnames(page_reviews.dt, c("ID", "user", "rating", "comment"))
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
toc()
reviews.dt[, `:=`(V1 = 0, name = "blank")]
setcolorder(reviews.dt, c("V1", "user", "rating", "comment", "ID", "name"))
all_reviews <- rbind(Reviews.dt, reviews.dt)

fwrite(reviews.dt, "New Reviews.csv")
fwrite(all_reviews, "All Reviews.csv")
