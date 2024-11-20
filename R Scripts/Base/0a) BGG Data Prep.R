#-------------------
# Loading the Data -
#-------------------

# Installing necessary packages
library("data.table")

setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Raw Data")
Reviews.dt <- fread("All Reviews.csv")
GameDetails.dt <- fread("games_detailed_info.csv")
Games.dt <- fread("games.csv")

#----------------
# Data Cleaning -
#----------------

# Removing duplicated information from games.csv
GameDetails.dt[, c("V1", "primary", "Board Game Rank", "average", "bayesaverage", 
                    "thumbnail", "yearpublished", "usersrated", "type") := NULL]

# Removing unnecessary categories (Not part of Board Game Geek)
GameDetails.dt[, c("Accessory Rank", "Amiga Rank", "Arcade Rank", "Atari ST Rank", "Commodore 64 Rank", "RPG Item Rank", "Video Game Rank"):= NULL]

# Creating a second detail table to do advanced manipulations on
GameDetails2.dt <- GameDetails.dt[, c("id", "boardgameartist", "boardgamedesigner", "boardgamecategory", "boardgamecompilation", 
                                        "boardgameexpansion", "boardgamefamily", "boardgameimplementation", "boardgameintegration", 
                                        "boardgamemechanic", "boardgamepublisher", "suggested_language_dependence", "suggested_num_players", "suggested_playerage")]
# Removing columns from GameDetails.csv
GameDetails.dt[, c("alternate", "boardgameartist", "boardgamedesigner", "boardgamecategory", "boardgamecompilation", 
                    "boardgameexpansion", "boardgamefamily", "boardgameimplementation", "boardgameintegration", 
                    "boardgamemechanic", "boardgamepublisher", "suggested_language_dependence", "suggested_num_players", "suggested_playerage") := NULL]

# Changing the order to be easier to understand
setcolorder(GameDetails.dt, c("id", "description", "median", "stddev", "averageweight", "numweights", "numcomments", "owned", "trading", "wanting", "wishing",
                               "minplayers", "maxplayers", "minage", "playingtime", "minplaytime", "maxplaytime"))

# Making the names more exact
setnames(GameDetails.dt, c("ID", "Description", "MedianRank", "StDevRank", "AverageWeight", "NumWeights", "NumComments", 
                            "Owned", "Trading", "Wanting", "Wishing", "MinPlayers", "MaxPlayers", "MinAge", "PlayTime", "MinPlayTime", "MaxPlayTime",
                            "Abstract", "Childrens", "Customizable", "Family", "Party", "Strategy", "Thematic", "Wargame", "Image"))

setnames(GameDetails2.dt, c("ID", "Artist", "Designer", "Category", "Compilation", "Expansion", "Family", "Implementation", 
                             "Integration", "Mechanic", "Publisher", "SuggestedLanguageDependence", "SuggestedNumPlayers", "SuggestedPlayerAge"))

# Setting new working directory for new tables
setwd("C:/Users/peter.matson/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Prepped Data")

# Writing out the reduced detail table
fwrite(GameDetails.dt, "GameDetails.csv")

#------------------------------------------------
# Creating Seperate Tables for each List column -
#------------------------------------------------

# Refining Game Details again
GameDetails3.dt <- GameDetails2.dt[, -c("SuggestedLanguageDependence", "SuggestedNumPlayers", "SuggestedPlayerAge")]

# Initializing Variables
i <- 2

# Starting loop
for (i in 2:length(GameDetails3.dt)) {
# Getting two columns to expand the data set
  y <- c(1, i)
# Creating temp table with only those columns
  temp.dt <- GameDetails3.dt[, .SD, .SDcols = y]
# Pulling out specific column name
  out <- names(temp.dt)[2]
# Setting specific column name to "x"
  setnames(temp.dt, c("ID", "x"))
# Removing strings of Quotes
  temp.dt[, x:=gsub('""', '', x)]
# Converting Python string to R list
  temp.dt[, x:=strsplit(gsub("\\[|\\]|\\'",'', x),', ')]
# Unlisting R list to create long dataset
  temp.dt <- temp.dt[,.(x = unlist(x)), by = ID]
# Re-setting column name back to original  
  setnames(temp.dt, c("ID", out))
# Writing out the new table
  fwrite(temp.dt, paste0(out, ".csv"))
# Iterate
  i <- i+1
}
rm(i, out, y, temp.dt, GameDetails3.dt)

# Reading in the New Tables
Artist.dt <- fread("Artist.csv")
# Category.dt <- fread("Category.csv")
# Compilation.dt <- fread("Compilation.csv")
Designer.dt <- fread("Designer.csv")
# Expansion.dt <- fread("Expansion.csv")
Family.dt <- fread("Family.csv")
# Implementation.dt <- fread("Implementation.csv")
# Integration.dt <- fread("Integration.csv")
# Mechanic.dt <- fread("Mechanic.csv")
Publisher.dt <- fread("Publisher.csv")

#---------------------------
# Data Cleaning New Tables -
#---------------------------

# Cleaning Out Artists of Bad Info
Artist.dt[, Name2:=shift(Artist, type = "lead"), by = ID]
Artist.dt[Name2 %in% c("Jr.", "III"), Artist:=paste(Artist, Name2)]
Artist.dt[, Name2 := NULL]
Artist.dt <- Artist.dt[!(Artist %in% c("Jr.", "III"))]
fwrite(Artist.dt, "Artist.csv")

# Cleaning Out Designer of Bad Info
Designer.dt[, Name2:=shift(Designer, type = "lead"), by = ID]
Designer.dt[Name2 %in% c("Jr.", "III"), Designer:=paste(Designer, Name2)]
Designer.dt[, Name2 := NULL]
Designer.dt <- Designer.dt[!(Designer %in% c("Jr.", "III"))]
fwrite(Designer.dt, "Designer.csv")

# Cleaning Out Family of Bad Info
Family.dt[, Name2:=shift(Family, type = "lead"), by = ID]
Family.dt[Family %like% '\\(' & !(Family %like% '\\)'),  Family:=paste(Family, ', ', Name2)]
Family.dt[, Name2 := NULL]
Family.dt <- Family.dt[!(!(Family %like% '\\(') & Family %like% '\\)')]
fwrite(Family.dt, "Family.csv")

# Cleaning Out Publisher of Bad Info
Publisher.dt <- Publisher.dt[!(Publisher %in% c("Inc.", "Inc.)", "Ltd.", "Ltd. (国際通信社)", "LLC."))]
fwrite(Publisher.dt, "Publisher.csv")

#--------------------------------
# Creating Major Category Table -
#--------------------------------

# Adding Major Cateogry as a table
MajorCategory.dt <- GameDetails.dt[, c("ID", "Abstract", "Childrens", "Customizable", "Family", "Party", "Strategy", "Thematic", "Wargame")]
# Creating a Long Data.Table and removing the Null values
MajorCategory.dt <- melt(MajorCategory.dt, id.vars = "ID", value.name = "CategoryRank", variable.name = "Category", na.rm = T)
# Writing out the data
fwrite(MajorCategory.dt, "MajorCategory.csv")

# Removing Major Category Ranks from the Details Table
GameDetails.dt[, c("Abstract", "Childrens", "Customizable", "Family", "Party", "Strategy", "Thematic", "Wargame"):=NULL]

#---------------------------------------------
# Data Cleaning for Ordered Dictonary Values -
#---------------------------------------------

# Getting Ordered Dictionary Columns (Python)
GameDetails3.dt <- GameDetails2.dt[, c("ID", "SuggestedLanguageDependence", "SuggestedNumPlayers", "SuggestedPlayerAge")]

# Getting Suggested Language Dependence Table Created
LanguageDependence.dt <- GameDetails3.dt[, c("ID", "SuggestedLanguageDependence")]
out <- names(LanguageDependence.dt)[2]
# Splitting out Ordered Dictionary into substrings of relevant information
LanguageDependence.dt[, x:=list(strsplit(gsub("^'|'$", '',gsub("\\@|\\[|\\]|\\(|\\)|\\,|OrderedDict", '',get(out))), "' '"))]
# Creating long data table of all the values
LanguageDependence.dt <- LanguageDependence.dt[,.(x = unlist(x)), by = ID]
# Shifting values to align values with key words
LanguageDependence.dt[, value:=shift(x, type = "lead"), by = ID]
# Filtering to only key word rows
LanguageDependence.dt <- LanguageDependence.dt[x %in% c("value", "numvotes")]
# Creating key to self join on
LanguageDependence.dt[, key:=ceiling(.I/2)]
# Self Joining to create final table
LanguageDependence.dt <- LanguageDependence.dt[x == "value", c(1,3,4)][LanguageDependence.dt[x == "numvotes", c(3,4)], on = "key"]
# Renaming columns and removing self join key
setnames(LanguageDependence.dt, c("ID", out, "key", "NumberOfVotes"))
LanguageDependence.dt[, key:=NULL]
# Writing out the data
fwrite(LanguageDependence.dt, "LanguageDependence.csv")

# Getting Suggested Player Age Table Created
PlayerAge.dt <- GameDetails3.dt[, c("ID", "SuggestedPlayerAge")]
out <- names(PlayerAge.dt)[2]
# Splitting out Ordered Dictionary into substrings of relevant information
PlayerAge.dt[, x:=list(strsplit(gsub("^'|'$", '',gsub("\\@|\\[|\\]|\\(|\\)|\\,|OrderedDict", '',get(out))), "' '"))]
# Creating long data table of all the values
PlayerAge.dt <- PlayerAge.dt[,.(x = unlist(x)), by = ID]
# Shifting values to align values with key words
PlayerAge.dt[, value:=shift(x, type = "lead"), by = ID]
# Filtering to only key word rows
PlayerAge.dt <- PlayerAge.dt[x %in% c("value", "numvotes")]
# Creating key to self join on
PlayerAge.dt[, key:=ceiling(.I/2)]
# Self Joining to create final table
PlayerAge.dt <- PlayerAge.dt[x == "value", c(1,3,4)][PlayerAge.dt[x == "numvotes", c(3,4)], on = "key"]
# Renaming columns and removing self join key
setnames(PlayerAge.dt, c("ID", out, "key", "NumberOfVotes"))
PlayerAge.dt[, key:=NULL]
# Writing out the data
fwrite(PlayerAge.dt, "PlayerAge.csv")

# Getting Suggested Number of Players Table Created
NumberOfPlayers.dt <- GameDetails3.dt[, c("ID", "SuggestedNumPlayers")]
out <- names(NumberOfPlayers.dt)[2]
# Splitting out Ordered Dictionary into substrings of relevant information
NumberOfPlayers.dt[, x:=list(strsplit(gsub("^'|'$", '',gsub("\\@|\\[|\\]|\\(|\\)|\\,|OrderedDict", '',get(out))), "' '"))]
# Creating long data table of all the values
NumberOfPlayers.dt <- NumberOfPlayers.dt[,.(x = unlist(x)), by = ID]
# Shifting values to align values with key words
NumberOfPlayers.dt[, `:=`(value=shift(x, type="lead"), value2=shift(x, n=2, type="lead")), by = ID]
# Filtering to only key word rows
NumberOfPlayers.dt <- NumberOfPlayers.dt[x %in% c("numplayers", "Best", "Recommended", "Not Recommended")]
# Creating key to self join on
NumberOfPlayers.dt[, key:=ceiling(.I/4)]
# Self Joining to create final table
NumberOfPlayers.dt <- NumberOfPlayers.dt[x == "numplayers", c(1,3,5)][NumberOfPlayers.dt[x != "numplayers", c(2,4,5)], on = "key"]
# Renaming columns and removing self join key
setnames(NumberOfPlayers.dt, c("ID", out, "key", "Level", "NumberOfVotes"))
NumberOfPlayers.dt[, key:=NULL]
# Writing out the data
fwrite(NumberOfPlayers.dt, "NumberOfPlayers.csv")

#---------------------
# Prepping Game Data -
#---------------------

# Cleaning up column names
setnames(Games.dt, c("ID", "Board Game", "Year Published", "Rank", "Overall Rating", "Geek Score", "Number of Ratings", "URL", "Thumbnail"))
# Filling out full URL
Games.dt[, URL:= paste0("https://boardgamegeek.com", URL)]
Games.dt[, DataDate:=as.Date("2019-05-02")]
# Adding in Game Details to remove redundant tables
Games.dt <- Games.dt[GameDetails.dt, on="ID"]

# Removing extra quotation marks from names
Games.dt[, `Board Game`:=gsub('""', '', `Board Game`)]
# Setting ancient game year as negative
Games.dt[`Board Game` %in% c('Go', 'Backgammon', 'Senet', 'Marbles'), `Year Published`:=-`Year Published`]
# Adding in Year Published to get full name
Games.dt[, `Board Game Name Full`:=paste0(`Board Game`, " (", `Year Published`, ")")]
# Setting order to group games by Full Name and order by ID
Games.dt <- Games.dt[order(`Board Game Name Full`, `ID`)]
# Counting distinct Board Game, Year Published combinations and setting order of publishing by ID.
Games.dt[, `:=`(Order = seq_len(.N), Distinct = .N), by=`Board Game Name Full`]
# Adding in publishing order (est) to Full Board Game Names when more than board game was published in the same year with the same name.
Games.dt[Distinct == 2, `Board Game Name Full`:=paste0(`Board Game`, "[", Order,"] (", `Year Published`, ")")]
# Removing the order column
Games.dt[,Order:=NULL]

# Removing page breaks and replacing them with a html break
Games.dt[, Description:=gsub('&#10;', "<br></br>", Description)]
Games.dt[, Description:=gsub("<br></br><br></br>", "<br></br>", Description)]

# Writing out the data
fwrite(Games.dt, "Games.csv")
