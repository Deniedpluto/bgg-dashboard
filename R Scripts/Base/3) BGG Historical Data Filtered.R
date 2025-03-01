# What does this Script do?

# This looks at all the ids in the top 500 and wow/mom data and simplifies the historic data pull to only those ids

# inputs
# MoM All Data and WoW All Data
# top_500_all_categories_reduced

# Output 
# historic_data_reduced (read in to Power BI)
# Top Plus MoM & WoW (read in to Power BI)

#-------------------
# Loading the Data -
#-------------------

# Installing necessary packages
library("data.table")
library("httr")
library("xml2")
library("readr")
library("lubridate")

#--------------------------------
# Reading in Historic Rank Data -
#--------------------------------

setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Historical Ranks") # setting the working directory
file_list <- list.files("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Historical Ranks", pattern = ".csv") # creating a list of all csv's in the directory

historic_rank.dt <- data.table() # initializing a blank data table
i = 1 # setting the start point to the first file in the list

for (i in 1:length(file_list)) {
  temp <- fread(file_list[i]) # reading in the file from the directory
  historic_rank.dt <- rbind(historic_rank.dt, temp) # appending the file to the files already pulled
}
rm(temp) # removing temp

#-----------------------
# Creating List of Ids -
#-----------------------

setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Historical Ranks/Over Time/Major Categories") # setting the working directory
wow_data <- fread("WoW All Data.csv") # reading in Week over Week data 
mom_data <- fread("MoM All Data.csv") # reading in Month over Month data
wow_data <- wow_data[!(Type %in% c("Decrease in Users Rated", "Decrease in Users Rated Percent"))]
mom_data <- mom_data[!(Type %in% c("Decrease in Users Rated", "Decrease in Users Rated Percent"))]
wow_ids <- unique(wow_data[, ID]) # getting list of unique ids from Week over Week
mom_ids <- unique(mom_data[, ID]) # getting list of unique ids from Month over Month

mom_wow_ids <- c(wow_ids, mom_ids) # combining month over month and week over week ids
mom_wow_ids <- unique(mom_wow_ids) # reducing list to only unique values

fwrite(data.table(mom_wow_ids), "mom_wow_ids.csv") # writing out list of all ids

setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Historical Ranks/Major Categories") # setting the working directory
top_500_all <- fread("top_500_all_categories_reduced.csv") # write out the data
top_500_ids <- unique(top_500_all[, ID])

all_ids <- c(top_500_ids, mom_wow_ids) # combining top 500 id list with all 
all_ids <- unique(all_ids) # reducing list to only unique values

#-------------------------
# Reducing Historic Data -
#-------------------------

historic_reduced <- historic_rank.dt[ID %in% all_ids] #reducing historic data to only those that make it into the top 500 or WoW/MoM 25
setnames(historic_reduced, c("ID", "Rank", "Rating", "Geek Score", "Users Rated", "Date")) # renaming columns for consistency
historic_reduced <- historic_reduced[wday(Date) == 1] # filtering data to only the first day of the week
fwrite(historic_reduced, "historic_data_reduced.csv") # writing out the data

#--------------------------------
# Combined Top and WoW/MoM Data -
#--------------------------------

top_500_all[, `:=`(Type = "Top 500", Comparison = "WoW")] # assiging the tag "Top 500" to all the top 500 games and comparison to "WoW"
setnames(top_500_all, c("ID", "Type Rank", "Date", "Major Category", "Type", "Comparison")) # renaming columns for consistency

mom_data[, Comparison:="MoM"]
wow_data[, Comparison:="WoW"]

combined_output <- rbind(wow_data, mom_data, top_500_all) # combining Top 500, 25 WoW, and 25 MoM
fwrite(combined_output, "Top Plus MoM & WoW.csv") # Writing out the data
