#-------------------
# Loading the Data -
#-------------------

# Installing necessary packages
library("data.table") # for efficient data manipulation

# Reading in the data
setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Prepped Data")
Reviews.dt <- fread("Reviews.csv")

Reviews.dt[, Round_Rating := round(Rating, 0)]
dim_reviews <- Reviews.dt[, .(number = .N, commented = sum(HasComment), ks = sum(Kickstarted)), by = c("ID", "Round_Rating")]
dim_reviews[, `:=`(pure_rating = number - commented, pure_comment = commented - ks)]
setnames(dim_reviews, c("ID", "Rating", "Total Ratings", "Comments", "Kickstarter", "Rating Only", "Comment Only"))
fwrite(dim_reviews, "Dim Reviews.csv")

Comments <- Reviews.dt[HasComment == 1]
Comments[, comment_length := nchar(Comment)]
# Comments[, comment_rank := frank(comment_length, ties.method = "first"), by = c("ID","Round_Rating")]
Comments.Reduced <- setorderv(Comments, c("ID", "Round_Rating", "comment_length"), c(1, 1, -1))[Lang == "en", head(.SD, 10), by = c("ID", "Round_Rating")]
Comments.Reduced[, c("Rating", "HasComment", "Kickstarted", "Lang", "comment_rank") := NULL]

fwrite(Comments.Reduced, "Reduced Comments.csv")
