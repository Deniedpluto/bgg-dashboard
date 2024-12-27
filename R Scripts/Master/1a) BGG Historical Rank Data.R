# What does this Script do?

# This script pull new historic data and adds it to existing files
# This is the basis for all other historic data pulls

# inputs
# Beefsack github repository
# current year historic data files

# Output 
# current year historic data files with new days added

#----------------------
# Setting up Packages -
#----------------------

# Installing necessary packages
library("httr")
library("data.table")
library("tictoc")
library("readr")
library("lubridate")

current_year <- year(Sys.Date() - 1)
last_year <- current_year - 1

# Getting the BGG Ranking Historical from 
beefsack <- "https://raw.githubusercontent.com/beefsack/bgg-ranking-historicals/master/" # URL to be combined with date to get the csv file
max_date <- as.character(Sys.Date()) # Today's date
min_date <- "2016-10-12" # first day of historical data from user/beefsack


setwd("C:/Users/Matso/source/repos/Deniedpluto/BGG-Data/Historical Ranks") # Directory in which everything is saved
historical_rank.dt <- try(fread(paste0("historical_ranks_", current_year, ".csv")), silent = TRUE) # Reading in Current year data or returning error if the file is missing
if('try-error' %in% class(historical_rank.dt)) {
  historical_rank.dt <- fread(paste0("historical_ranks_", last_year, ".csv")) # Returing last years data if the current year is missing
}

historical_rank.dt[, date:=as.Date(date)] # Converting the date column to be a date

last_date_pulled <- historical_rank.dt[, .(max(date))][[1]] # Getting the last date from the file

data_date <- last_date_pulled + 1 # Setting the starting date for the new data pull
if(month(data_date)==1 & day(data_date)==1) {
  historical_rank.dt <- data.table()
}

#-----------------
# Data Pull Loop -
#-----------------

tic("Updating historical ranks data")
while(data_date <= as.Date(max_date))   {
  beefsack_api <- paste0(beefsack, data_date, ".csv") # Setting up the URL to be read in
  tryCatch(
    beefsack_data <- data.table(read_csv(beefsack_api)), # Reading in the URL
    #warning = function(w) {print(paste("Warning for date:", data_date))},
    error = function(w) {print(paste("Error for date", data_date, ". Record does not exist."))}
  )
  beefsack_data[, c("Name", "Year", "URL", "Thumbnail") := NULL] # Removing duplicating data
  beefsack_data[, date:=data_date] # Adding the date column to the table
  
  historical_rank.dt <- rbind(beefsack_data, historical_rank.dt) # Adding the new data pull to the historicals
  data_date = data_date + 1 # updating the date
}
toc()

#----------------------
# Removing Duplicates -
#----------------------

historical_rank.dt[, index := 1:.N, by = c("ID", "date")] # setting the index to use ID and date
historical_rank.dt <- historical_rank.dt[index == 1] # filtering to only the first observation by ID and date
historical_rank.dt[, index := NULL] # removing the index column

fwrite(historical_rank.dt[year(date) == current_year], paste0("historical_ranks_",  current_year, ".csv")) # Writing the current year data
if(nrow(historical_rank.dt[year(date) == last_year]) > 0) {
fwrite(historical_rank.dt[year(date) == last_year], paste0("historical_ranks_",  last_year, ".csv")) # Writing out last years data
}

