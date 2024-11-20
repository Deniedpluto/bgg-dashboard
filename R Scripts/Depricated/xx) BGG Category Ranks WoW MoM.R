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

setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Historical Ranks") # setting the working directory
file_list <- list.files("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Historical Ranks", pattern = ".csv") # creating a list of all csv's in the directory

historic_rank.dt <- data.table() # initializing a blank data table
i = 1 # setting the start point to the first file in the list

for (i in 1:length(file_list)) {
  temp <- fread(file_list[i]) # reading in the file from the directory
  historic_rank.dt <- rbind(historic_rank.dt, temp) # appending the file to the files already pulled
}
rm(temp) # removing temp

historic_ids <- as.list(unique(historic_rank.dt[, ID])) # getting a list of all IDs every pulled

#---------------------------
# Creating List of New Ids -
#---------------------------

beefsack <- "https://raw.githubusercontent.com/beefsack/bgg-ranking-historicals/master/" # Beefsack's github URL
beefsack_date <- as.character(Sys.Date()) # Date to be passed to the beefsack url
beefsack_api <- paste0(beefsack, beefsack_date, ".csv") # Complete URL to be read in

beefsack_data <- data.table(read_csv(beefsack_api)) # Pulling in the beefsack data
id_list <- as.list(beefsack_data[, ID]) # Creating a list of all the ids
rm(beefsack, beefsack_api, beefsack_date, beefsack_data) # removing beefsack url strings

setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Historical Ranks/Major Categories") # setting the working directory

all_ids <- c(historic_ids, id_list)
all_ids <- data.table(all_ids)
all_ids[, all_ids := as.integer(all_ids)]
all_ids <- unique(all_ids)

fwrite(all_ids, "beefsack_ids.csv") # overwriting old list with new list

new_ids <- setdiff(id_list, historic_ids) # finding the list of new ids that are not in the prior list

#---------------------------
# Getting Major Categories -
#---------------------------

iterations = ceiling(length(new_ids)/500) - 1 
i = 0

major_category_list <- data.table()


for(i in 0:iterations) {
  bgg1 <- "https://www.boardgamegeek.com/xmlapi2/thing?id="
  bgg2 <- "&stats=1"
  active_ids <- paste0(new_ids[((i*500)+1):((i+1)*500)], sep = ",", collapse = "")
  bgg_api <- paste0(bgg1, active_ids, bgg2)
  
  bgg_data <- read_xml(bgg_api)
  
  ids <- as.list(xml_attr(xml_find_all(bgg_data, "//item"), "id"))
  major_cat <- xml_find_all(bgg_data, "//ranks")
  
  full_ids <- rep(ids,  xml_length(major_cat))  
  full_major_cat <- xml_attr(xml_find_all(major_cat, "//rank"), "friendlyname")
  temp <- cbind(data.table(full_ids), data.table(full_major_cat))
  
  major_category_list <- rbind(major_category_list, temp)
  wait(1)
  i = i + 1 
}
rm(temp)

major_category_list[, full_ids:=as.character(full_ids)]
major_category_list[, full_major_cat:=gsub(' Rank', '', full_major_cat)]
major_category_list[, full_major_cat:=gsub(' Game', '', full_major_cat)]
major_category_list <- major_category_list[full_major_cat!="Board"]
major_category_list <- major_category_list[!(full_major_cat %in% c("RPG Item", "Accessory", "Video", "Amiga", "Commodore 64", "Arcade", "Atari ST"))]

setnames(major_category_list, c("ID", "Major Category"))

old_major_category_list <- fread("major_category_list.csv")

major_category_list <- rbind(major_category_list, old_major_category_list)
major_category_list <- unique(major_category_list)

fwrite(major_category_list, "major_category_list.csv")

major_categories <- unique(major_category_list[, `Major Category`])

#-------------------------
# Creating Top 500 Lists -
#-------------------------

top_500 <- historic_rank.dt[Rank <= 500]
fwrite(top_500, "top_500_overall.csv")

# Thematic
thematic_ids <- unique(major_category_list[`Major Category`=="Thematic", ID])
historic_rank_thematic <- historic_rank.dt[ID %in% thematic_ids]
historic_rank_thematic[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date]
top_500_thematic <- historic_rank_thematic[new_rank <= 500]
top_500_thematic[, `Major Category`:="Thematic" ]
fwrite(top_500_thematic, "top_500_thematic.csv")

# Strategy
strategy_ids <- unique(major_category_list[`Major Category`=="Strategy", ID])
historic_rank_strategy <- historic_rank.dt[ID %in% strategy_ids]
historic_rank_strategy[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date]
top_500_strategy <- historic_rank_strategy[new_rank <= 500]
top_500_strategy[, `Major Category`:="Strategy" ]
fwrite(top_500_strategy, "top_500_strategy.csv")

# Family
family_ids <- unique(major_category_list[`Major Category`=="Family", ID])
historic_rank_family <- historic_rank.dt[ID %in% family_ids]
historic_rank_family[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date]
top_500_family <- historic_rank_family[new_rank <= 500]
top_500_family[, `Major Category`:="Family" ]
fwrite(top_500_family, "top_500_family.csv")

# War
war_ids <- unique(major_category_list[`Major Category`=="War", ID])
historic_rank_war <- historic_rank.dt[ID %in% war_ids]
historic_rank_war[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date]
top_500_war <- historic_rank_war[new_rank <= 500]
top_500_war[, `Major Category`:="War" ]
fwrite(top_500_war, "top_500_war.csv")

# Abstract
abstract_ids <- unique(major_category_list[`Major Category`=="Abstract", ID])
historic_rank_abstract <- historic_rank.dt[ID %in% abstract_ids]
historic_rank_abstract[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date]
top_500_abstract <- historic_rank_abstract[new_rank <= 500]
top_500_abstract[, `Major Category`:="Abstract" ]
fwrite(top_500_abstract, "top_500_abstract.csv")

# Party
party_ids <- unique(major_category_list[`Major Category`=="Party", ID])
historic_rank_party <- historic_rank.dt[ID %in% party_ids]
historic_rank_party[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date]
top_500_party <- historic_rank_party[new_rank <= 500]
top_500_party[, `Major Category`:="Party" ]
fwrite(top_500_party, "top_500_party.csv")

# Customizable
customizable_ids <- unique(major_category_list[`Major Category`=="Customizable", ID])
historic_rank_customizable <- historic_rank.dt[ID %in% customizable_ids]
historic_rank_customizable[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date]
top_500_customizable <- historic_rank_customizable[new_rank <= 500]
top_500_customizable[, `Major Category`:="Customizable"]
fwrite(top_500_customizable, "top_500_customizable.csv")

# Children's
childrens_ids <- unique(major_category_list[`Major Category`=="Children's", ID])
historic_rank_childrens <- historic_rank.dt[ID %in% childrens_ids]
historic_rank_childrens[, new_rank := rank(-`Bayes average`, ties.method = "first"), by = date]
top_500_childrens <- historic_rank_childrens[new_rank <= 500]
top_500_childrens[, `Major Category`:="Children's"]
fwrite(top_500_childrens, "top_500_childrens.csv")

#--------------------------
# Putting It All Together -
#--------------------------

top_500[, `:=`(new_rank=Rank, `Major Category`="")]

all_top_500 <- rbindlist( list(top_500, top_500_abstract, top_500_childrens, top_500_customizable, top_500_family, top_500_party, top_500_strategy, top_500_thematic, top_500_war))
fwrite(all_top_500, "top_500_all_categories.csv")
