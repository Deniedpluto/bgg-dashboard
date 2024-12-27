# What does this Script do?

# This script pulls in all the historic data collected finds the top 500 games for each major category per day

# inputs
# all historic data files
# beefsack's most recent file output

# Output 
# major_categories_list
# beefsack_ids
# all top_500 files
    # top_500_all_categories_reduced (most important file)

#-------------------
# Loading the Data -
#-------------------

# Installing necessary packages
library("data.table")
library("httr")
library("xml2")
library("readr")

wait <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1
}
#--------------------------------
# Reading in Historic Rank Data -
#--------------------------------

# historic_rank.dt <- historical_rank.dt

setwd("C:/Users/Matso/source/repos/Deniedpluto/BGG-Data/Historical Ranks") # setting the working directory
file_list <- list.files("C:/Users/Matso/source/repos/Deniedpluto/BGG-Data/Historical Ranks", pattern = ".csv") # creating a list of all csv's in the directory

historic_rank.dt <- data.table() # initializing a blank data table
i = 1 # setting the start point to the first file in the list

for (i in 1:length(file_list)) {
  temp <- fread(file_list[i]) # reading in the file from the directory
  historic_rank.dt <- rbind(historic_rank.dt, temp) # appending the file to the files already pulled
}
rm(temp) # removing temp

#---------------------------
# Creating List of New Ids -
#---------------------------

beefsack <- "https://raw.githubusercontent.com/beefsack/bgg-ranking-historicals/master/" # Beefsack's github URL
beefsack_date <- as.character(Sys.Date()) # Date to be passed to the beefsack url
beefsack_api <- paste0(beefsack, beefsack_date, ".csv") # Complete URL to be read in

beefsack_data <- data.table(read_csv(beefsack_api)) # Pulling in the beefsack data
id_list <- unique(beefsack_data[, ID]) # Creating a list of all the ids
rm(beefsack, beefsack_api, beefsack_date, beefsack_data) # removing beefsack url strings

setwd("C:/Users/Matso/source/repos/Deniedpluto/BGG-Data/Historical Ranks/Major Categories") # setting the working directory
#historic_ids <- unique(historic_rank.dt[, ID]) # getting a list of all IDs every pulled
historic_ids <- unique(fread("beefsack_ids.csv")[, all_ids]) # reading in list of all historic ids

all_ids <- c(historic_ids, id_list) # combining old and new list
all_ids <- unique(all_ids) # removing duplicates

fwrite(data.table(all_ids), "beefsack_ids.csv") # overwriting lits of historic ids with new combined list

new_ids <- setdiff(id_list, historic_ids) # finding the list of new ids that are not in the prior list

#---------------------------
# Getting Major Categories -
#---------------------------

iterations = ceiling(length(new_ids)/20) - 1 # create a number of iterations based on groups of 500 ids
i = 0 # setting the start value at 0 (since 0 * 500 + 1 = 1)

major_category_list <- data.table() # initializing a blank data table.

bgg1 <- "https://www.boardgamegeek.com/xmlapi2/thing?id=" # getting the first part of the bgg xmlapi2 url
bgg2 <- "&stats=1" # setting to bgg xmlapi2 to retrieve stats (for major category)


if (length(new_ids) > 0) {
  for(i in 0:iterations) {
    pull_id_list <- new_ids[((i*20)+1):((i+1)*20)] # reducing the list to 20 (or less) ids
    pull_id_list <- pull_id_list[!sapply(pull_id_list, is.na)] # removing NAs from the list 
    active_ids <- paste0(pull_id_list, sep = ",", collapse = "") # converting the list to text
    active_ids <- substr(active_ids, 1, nchar(active_ids)-1) # removing the trailing ,
    bgg_api <- paste0(bgg1, active_ids, bgg2) # setting 500 ids inside the bgg xmlapi2 call
    
    bgg_data <- read_xml(bgg_api) # reading in the xml data
    
    ids <- as.list(xml_attr(xml_find_all(bgg_data, "//item"), "id")) # parsing the data to item ids
    major_cat <- xml_find_all(bgg_data, "//ranks") # reducing the xml to only rank (category) data
    
    full_ids <- rep(ids,  xml_length(major_cat)) # repeating ids based on the number of categories (xml_length(major_cat))
    full_major_cat <- xml_attr(xml_find_all(major_cat, "//rank"), "friendlyname") # pulling in the fiendlyname of the category
    temp <- cbind(data.table(full_ids), data.table(full_major_cat)) # combining the id with the category
    
    major_category_list <- rbind(major_category_list, temp) # combining new pulls with existing pulls
    print(paste("Pulled", i+1, "of", iterations + 1, "lists pulled."))
    wait(8) # wait 8 seconds
    i = i + 1  # increase i to next iteration
  }
  rm(temp) # remove duplicate temp data
  
  major_category_list[, full_ids:=as.character(full_ids)] # convert ids from integers to characters
  major_category_list[, full_major_cat:=gsub(' Rank', '', full_major_cat)] # removing the word Rank from category
  major_category_list[, full_major_cat:=gsub(' Game', '', full_major_cat)] # removing the word Game from category
  major_category_list <- major_category_list[full_major_cat!="Board"] # removing the "Board" category (i.e. overall rank)
  major_category_list <- major_category_list[!(full_major_cat %in% c("RPG Item", "Accessory", "Video", "Amiga", "Commodore 64", "Arcade", "Atari ST"))] # removing unrelated categories
  
  setnames(major_category_list, c("ID", "Major Category")) # renaming columns
  } 

old_major_category_list <- fread("major_category_list.csv") # reading in prior major category lists 

major_category_list <- rbind(major_category_list, old_major_category_list) # combining new and old lists
major_category_list <- unique(major_category_list) # removing duplicates

fwrite(major_category_list, "major_category_list.csv") # overwriting the old list with the new complete list of major categories

major_categories <- unique(major_category_list[, `Major Category`]) # creating a list of major categories 


#-------------------------
# Creating Top 500 Lists -
#-------------------------47
  
top_500 <- historic_rank.dt[Rank <= 500] # create table of top 500 games for every day
fwrite(top_500, "top_500_overall.csv") # write out the data

top_500[, `:=`(new_rank=Rank, `Major Category`="Overall")] # add new_rank = Rank and set major category to "Overall" for the top 500 overall games.

i = 1
category <- major_categories[i]
all_top_500 <- top_500


for(category in major_categories) { # This uses the thematic category as an example.
  category_ids <- unique(major_category_list[`Major Category`==category, ID]) # filter ids to only those in major category "Thematic"
  historic_rank_category <- historic_rank.dt[ID %in% category_ids] # create table of historic data for thematic games
  historic_rank_category[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date] # rank the games within the thematic category
  top_500_category <- historic_rank_category[new_rank <= 500] # filter the thematic games to only the top 500
  top_500_category[, `Major Category`:=category] # add a major category label to label games as thematic
  fwrite(top_500_category, paste0("top_500_", category ,".csv")) # write out thematic games
  
  all_top_500 <- rbind(all_top_500, top_500_category)
  i = i + 1
  category <- major_categories[i]
}

#--------------------------
# Putting It All Together -
#--------------------------

# combine all top 500 lists together
fwrite(all_top_500, "top_500_all_categories.csv") # write out the data

all_top_500_reduced <- all_top_500[, c("ID", "new_rank", "date", "Major Category")] # reduce the columns to only the necessary columns
fwrite(all_top_500_reduced, "top_500_all_categories_reduced.csv") # write out the data


# # Thematic
# thematic_ids <- unique(major_category_list[`Major Category`=="Thematic", ID]) # filter ids to only those in major category "Thematic"
# historic_rank_thematic <- historic_rank.dt[ID %in% thematic_ids] # create table of historic data for thematic games
# historic_rank_thematic[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date] # rank the games within the thematic category
# top_500_thematic <- historic_rank_thematic[new_rank <= 500] # filter the thematic games to only the top 500
# top_500_thematic[, `Major Category`:="Thematic" ] # add a major category label to label games as thematic
# fwrite(top_500_thematic, "top_500_thematic.csv") # write out thematic games
# 
# # Strategy
# # See notes for thematic -> same steps but replace thematic with strategy
# strategy_ids <- unique(major_category_list[`Major Category`=="Strategy", ID])
# historic_rank_strategy <- historic_rank.dt[ID %in% strategy_ids]
# historic_rank_strategy[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date]
# top_500_strategy <- historic_rank_strategy[new_rank <= 500]
# top_500_strategy[, `Major Category`:="Strategy" ]
# fwrite(top_500_strategy, "top_500_strategy.csv")
# 
# # Family
# # See notes for thematic -> same steps but replace thematic with family
# family_ids <- unique(major_category_list[`Major Category`=="Family", ID])
# historic_rank_family <- historic_rank.dt[ID %in% family_ids]
# historic_rank_family[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date]
# top_500_family <- historic_rank_family[new_rank <= 500]
# top_500_family[, `Major Category`:="Family" ]
# fwrite(top_500_family, "top_500_family.csv")
# 
# # War
# # See notes for thematic -> same steps but replace thematic with war
# war_ids <- unique(major_category_list[`Major Category`=="War", ID])
# historic_rank_war <- historic_rank.dt[ID %in% war_ids]
# historic_rank_war[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date]
# top_500_war <- historic_rank_war[new_rank <= 500]
# top_500_war[, `Major Category`:="War" ]
# fwrite(top_500_war, "top_500_war.csv")
# 
# # Abstract
# # See notes for thematic -> same steps but replace thematic with abstarct
# abstract_ids <- unique(major_category_list[`Major Category`=="Abstract", ID])
# historic_rank_abstract <- historic_rank.dt[ID %in% abstract_ids]
# historic_rank_abstract[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date]
# top_500_abstract <- historic_rank_abstract[new_rank <= 500]
# top_500_abstract[, `Major Category`:="Abstract" ]
# fwrite(top_500_abstract, "top_500_abstract.csv")
# 
# # Party
# # See notes for thematic -> same steps but replace thematic with party
# party_ids <- unique(major_category_list[`Major Category`=="Party", ID])
# historic_rank_party <- historic_rank.dt[ID %in% party_ids]
# historic_rank_party[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date]
# top_500_party <- historic_rank_party[new_rank <= 500]
# top_500_party[, `Major Category`:="Party" ]
# fwrite(top_500_party, "top_500_party.csv")
# 
# # Customizable
# # See notes for thematic -> same steps but replace thematic with customizable
# customizable_ids <- unique(major_category_list[`Major Category`=="Customizable", ID])
# historic_rank_customizable <- historic_rank.dt[ID %in% customizable_ids]
# historic_rank_customizable[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date]
# top_500_customizable <- historic_rank_customizable[new_rank <= 500]
# top_500_customizable[, `Major Category`:="Customizable"]
# fwrite(top_500_customizable, "top_500_customizable.csv")
# 
# # Children's
# # See notes for thematic -> same steps but replace thematic with childrens
# childrens_ids <- unique(major_category_list[`Major Category`=="Children's", ID])
# historic_rank_childrens <- historic_rank.dt[ID %in% childrens_ids]
# historic_rank_childrens[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date]
# top_500_childrens <- historic_rank_childrens[new_rank <= 500]
# top_500_childrens[, `Major Category`:="Children's"]
# fwrite(top_500_childrens, "top_500_childrens.csv")