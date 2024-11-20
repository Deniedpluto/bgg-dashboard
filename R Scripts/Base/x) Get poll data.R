

library(data.table)
library(lubridate)
library(xml2)

# Setup sample data

setwd("C:/Users/Peter.Matson/OneDrive - Calligo Limited/Onboarding/Board Game Geek Dashboard/Historical Ranks") # Directory in which everything is saved
historical_rank.dt <- fread("historical_ranks_2019.csv") # Returing last years data if the current year is missing

historical_rank.dt[, date2 := day(date)]
subset <- historical_rank.dt[date2 == 1, ]

historical_rank.dt <- fread("historical_ranks_2020.csv") # Returing last years data if the current year is missing
historical_rank.dt[, date2 := day(date)]
subset2 <- historical_rank.dt[date2 == 1, ]

historical_rank.dt <- fread("historical_ranks_20.csv") # Returing last years data if the current year is missing
historical_rank.dt[, date2 := day(date)]
subset3 <- historical_rank.dt[date2 == 1, ]


los_dos <- rbind(subset, subset2)

fwrite(los_dos, "sample_data.csv")       


# Pull BGG data

setwd("C:/Users/Peter.Matson/OneDrive - Calligo Limited/Onboarding/Board Game Geek Dashboard/Historical Ranks") # setting the working directory
file_list <- list.files("C:/Users/Peter.Matson/OneDrive - Calligo Limited/Onboarding/Board Game Geek Dashboard/Historical Ranks", pattern = ".csv") # creating a list of all csv's in the directory

historic_rank.dt <- data.table() # initializing a blank data table
i = 1 # setting the start point to the first file in the list

for (i in 1:length(file_list)) {
  temp <- fread(file_list[i]) # reading in the file from the directory
  historic_rank.dt <- rbind(historic_rank.dt, temp) # appending the file to the files already pulled
}
rm(temp) 

all_ids <- unique(historic_rank.dt[, ID])

iterations = ceiling(length(all_ids)/500) - 1 # create a number of iterations based on groups of 500 ids
i = 0 # setting the start value at 0 (since 0 * 500 + 1 = 1)

bgg1 <- "https://www.boardgamegeek.com/xmlapi2/thing?id=" # getting the first part of the bgg xmlapi2 url
bgg2 <- "&stats=1" # setting to bgg xmlapi2 to retrieve stats (for major category)

for(i in 0:iterations) {
  i = i + 1
  active_ids <- paste0(all_ids[((i*500)+1):((i+1)*500)], sep = ",", collapse = "") # getting the groupings of 500 ids
  bgg_api <- paste0(bgg1, active_ids, bgg2) # setting 500 ids inside the bgg xmlapi2 call
  bgg_api
  
  bgg_data <- read_xml(bgg_api) # reading in the xml data
  test <- as_list(bgg_data)
  unlist(test[["items"]][["item"]][["poll"]][["results"]][["result"]])
  
  
  test <- as.list(xml_find_all(bgg_data, "//poll")) # parsing the data to item ids
  full_ids <- rep(rep(ids, 3), xml_length(test))
  a <- unique(xml_attrs(test))
  xml_find_all(test, "//suggest_numplayers")
  
  ids <- as.list(xml_attr(xml_find_all(bgg_data, "//item"), "id")) # parsing the data to item ids
  major_cat <- xml_find_all(bgg_data, "//ranks") # reducing the xml to only rank (category) data
  
  full_ids <- rep(ids,  xml_length(major_cat)) # repeating ids based on the number of categories (xml_length(major_cat))
  full_major_cat <- xml_attr(xml_find_all(major_cat, "//rank"), "friendlyname") # pulling in the fiendlyname of the category
  temp <- cbind(data.table(full_ids), data.table(full_major_cat)) # combining the id with the category
  
  major_category_list <- rbind(major_category_list, temp) # combining new pulls with existing pulls
  wait(1) # wait 5 seconds
  i = i + 1  # increase i to next iteration
}