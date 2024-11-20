# What does this Script do?

# This script pulls in all the historic data collected and aggregates it into usable information

# inputs
# all historic data files

# Output 
# dim_games (to be added to the live bgg Dim Games table)

#-------------------
# Loading the Data -
#-------------------

# Installing necessary packages
library("data.table")
library("lubridate")

#--------------------------------
# Reading in Historic Rank Data -
#--------------------------------

setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Historical Ranks")
file_list <- list.files("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Historical Ranks", pattern = ".csv")

historic_rank.dt <- data.table()

i = 1

for (i in 1:length(file_list)) {
  temp <- fread(file_list[i])
  historic_rank.dt <- rbind(historic_rank.dt, temp)
}
rm(temp)

#-------------------------
# Getting Dimension Data -
#-------------------------

historic_rank.dt[, date := as.Date(date)]

setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Historical Ranks/Over Time")
dim_games.dt <- historic_rank.dt[, .(inception_date = min(date), avg_rank = mean(Rank),
                                     min_rank = min(Rank), max_rank = max(Rank),
                                     min_rating = min(Average), max_rating = max(Average),
                                     min_geek_score = min(`Bayes average`), max_geek_score = max(`Bayes average`)), by = ID]
fwrite(dim_games.dt, "dim_games.csv")
