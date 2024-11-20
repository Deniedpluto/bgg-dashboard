# What does this Script do?

# This script pulls in all the historic data collected finds the top 25 games for each major category and change per day

# inputs
# all historic data files
# major_categories_list

# Output 
# Week over Week and Month over Month Data
  # WoW and MoM By Major Category and Date
    # MoM All Data and WoW All Data (most important)

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

#----------------------------------
# Creating Different Aggregations -
#----------------------------------

historic_rank.dt <- historic_rank.dt[, date := as.Date(date)]

historic_rank.dt[, date_week_plus := date + 7]
historic_rank.dt[, date_month_plus := date + 28]

historic_rank_test.dt[, .(#min_date = min(date), max_date = max(date),
                     min_date7 = min(date_week_plus), max_date7 = max(date_week_plus),
                     min_date30 = min(date_month_plus), max_date30 = max(date_month_plus))]

historic_rank_week_plus.dt <- historic_rank.dt[, c("ID", "Rank", "Average", "Bayes average", "Users rated", "date_week_plus")]
historic_rank_month_plus.dt <- historic_rank.dt[, c("ID", "Rank", "Average", "Bayes average", "Users rated", "date_month_plus")]


#---------------------------
# Setting Keys and Joining -
#---------------------------

setkeyv(historic_rank.dt, c("ID", "date"))
setkeyv(historic_rank_week_plus.dt, c("ID", "date_week_plus"))
setkeyv(historic_rank_month_plus.dt, c("ID", "date_month_plus"))

historic_rank_over_time.dt <- historic_rank_week_plus.dt[historic_rank.dt, ] 
historic_rank_over_time.dt <- historic_rank_month_plus.dt[historic_rank_over_time.dt, ] 

rm(historic_rank_month_plus.dt, historic_rank_week_plus.dt)

#--------------------------------------
# Renaming Columns and Getting Values -
#--------------------------------------

historic_rank_over_time.dt[, c("i.date_week_plus", "i.date_month_plus"):= NULL]

setnames(historic_rank_over_time.dt, c("ID", "Rank", "Average", "Bayes average", "Users rated", "date_month_plus",
                             "i.Rank", "i.Average", "i.Bayes average", "i.Users rated", 
                             "i.Rank.1", "i.Average.1", "i.Bayes average.1", "i.Users rated.1"),
                           c("ID", "prior_month_Rank", "prior_month_Rating", "prior_month_Geek_Score", "prior_month_Users_Rated", "Date",
                             "prior_week_Rank", "prior_week_Rating", "prior_week_Geek_Score", "prior_week_Users_Rated", 
                             "Rank", "Rating", "Geek_Score", "Users_Rated"))

setcolorder(historic_rank_over_time.dt, c("ID", "Rank", "Rating", "Geek_Score", "Users_Rated", "Date",
                                          "prior_week_Rank", "prior_week_Rating", "prior_week_Geek_Score", "prior_week_Users_Rated",
                                          "prior_month_Rank", "prior_month_Rating", "prior_month_Geek_Score", "prior_month_Users_Rated"))

historic_rank_over_time.dt[, `:=`(WoW_Rank = (prior_week_Rank - Rank), WoW_Rating = (Rating - prior_week_Rating),
                                  WoW_Geek_Score = (`Geek_Score` - `prior_week_Geek_Score`), WoW_Users_Rated = (`Users_Rated` - `prior_week_Users_Rated`),
                                  MoM_Rank = (prior_month_Rank - Rank), MoM_Rating = (Rating - prior_month_Rating),
                                  MoM_Geek_Score = (`Geek_Score` - `prior_month_Geek_Score`), MoM_Users_Rated = (`Users_Rated` - `prior_month_Users_Rated`))]

historic_rank_over_time.dt[, `:=`(WoW_Users_Rated_Percent = WoW_Users_Rated / prior_week_Users_Rated,
                                  MoM_Users_Rated_Percent = MoM_Users_Rated / prior_month_Users_Rated)]



monthly_data.dt <- historic_rank_over_time.dt[day(Date) == 1, c(1:6,19:22,24)]
# monthly_data.dt <- na.omit(monthly_data.dt)
weekly_data.dt <- historic_rank_over_time.dt[wday(Date) == 1, c(1:6,15:24)]
# weekly_data.dt <- na.omit(weekly_data.dt)

#--------------------------
# Setting Up WoW/MoM Data -
#--------------------------

setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Historical Ranks/Over Time")

n <- 25 # Number of Top Games Selected
type <- c("Rank", "Rating", "Geek Score", "Users Rated", "Users Rated Percent")
setnames(weekly_data.dt, c("ID", "Rank", "Rating", "Geek Score", "Users Rated", "Date",
                           "WoW Rank", "WoW Rating", "WoW Geek Score", "WoW Users Rated", 
                           "MoM Rank", "MoM Rating", "MoM Geek Score", "MoM Users Rated",
                           "WoW Users Rated Percent", "MoM Users Rated Percent"))

#-----------------------------------------
# Calculating Week over Week Top Changes -
#-----------------------------------------

# WoW Columns
cols <- c("ID", "Rank", "WoW Rank", "Rating", "WoW Rating", "Geek Score", "WoW Geek Score", "Users Rated", "WoW Users Rated", "WoW Users Rated Percent")
i = 1
set_type <- type[i]

WoW_Data <- data.table()

for(set_type in type) {
  primary <- paste0("WoW ", set_type)
  secondary <- if(set_type == "Geek Score") {"WoW Rating"} else {"WoW Geek Score"}
  temp <- na.omit(weekly_data.dt, cols=primary)
  top_n_type_increase_wow <- setorderv(temp, c(primary, secondary), c(-1, -1))[, head(.SD, n), by = Date, .SDcols = cols]
  top_n_type_increase_wow[, Type := paste0("Increase in ", set_type)]
  top_n_type_decrease_wow <- setorderv(temp, c(primary, secondary), c(1, 1))[, head(.SD, n), by = Date, .SDcols = cols]
  top_n_type_decrease_wow[, Type := paste0("Decrease in ", set_type)]
  WoW_type <- rbind(top_n_type_increase_wow, top_n_type_decrease_wow)
  rm(top_n_type_increase_wow, top_n_type_decrease_wow, temp)
  fwrite(WoW_type, paste0("WoW ", set_type, ".csv"))
  
  WoW_Data <- rbind(WoW_Data, WoW_type)
  i = i + 1
  set_type <- type[i]
}

WoW_Data[Type %like% "Rating", Value := `WoW Rating`]
WoW_Data[Type %like% "Rank", Value := `WoW Rank`]
WoW_Data[Type %like% "Geek Score", Value := `WoW Geek Score`]
WoW_Data[Type %like% "Users", Value := `WoW Users Rated`]
WoW_Data[Type %like% "Percent", Value := `WoW Users Rated Percent`]

WoW_Data[, `Type Rank` := frankv(abs(Value), order = -1, ties.method="first"), by = c("Date", "Type")]
fwrite(WoW_Data, "WoW Data.csv")

#-----------------------------------------
# Calculating Month over Month Top Changes -
#-----------------------------------------

# MoM Columns
cols <- c("ID", "Rank", "MoM Rank", "Rating", "MoM Rating", "Geek Score", "MoM Geek Score", "Users Rated", "MoM Users Rated", "MoM Users Rated Percent")
i = 1
set_type <- type[i]

MoM_Data <- data.table()

for(set_type in type) {
  primary <- paste0("MoM ", set_type)
  secondary <- if(set_type == "Geek Score") {"MoM Rating"} else {"MoM Geek Score"}
  temp <- na.omit(weekly_data.dt, cols=primary)
  top_n_type_increase_mom <- setorderv(temp, c(primary, secondary), c(-1, -1))[, head(.SD, n), by = Date, .SDcols = cols]
  top_n_type_increase_mom[, Type := paste0("Increase in ", set_type)]
  top_n_type_decrease_mom <- setorderv(temp, c(primary, secondary), c(1, 1))[, head(.SD, n), by = Date, .SDcols = cols]
  top_n_type_decrease_mom[, Type := paste0("Decrease in ", set_type)]
  MoM_type <- rbind(top_n_type_increase_mom, top_n_type_decrease_mom)
  rm(top_n_type_increase_mom, top_n_type_decrease_mom, temp)
  fwrite(MoM_type, paste0("MoM ", set_type, ".csv"))
  
  MoM_Data <- rbind(MoM_Data, MoM_type)
  i = i + 1
  set_type <- type[i]
}

MoM_Data[Type %like% "Rating", Value := `MoM Rating`]
MoM_Data[Type %like% "Rank", Value := `MoM Rank`]
MoM_Data[Type %like% "Geek Score", Value := `MoM Geek Score`]
MoM_Data[Type %like% "Users", Value := `MoM Users Rated`]
MoM_Data[Type %like% "Percent", Value := `MoM Users Rated Percent`]

MoM_Data[, `Type Rank` := frankv(abs(Value), order = -1, ties.method="first"), by = c("Date", "Type")]
fwrite(MoM_Data, "MoM Data.csv")


#------------------
# WoW By Category -
#------------------

setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Historical Ranks/Major Categories")
major_category_list <- fread("major_category_list.csv") # reading in prior major category lists 
major_categories <- unique(major_category_list[, `Major Category`]) # creating a list of major categories

setwd("C:/Users/v-pemats/OneDrive - Decisive Data/Onboarding/Board Game Geek Dashboard/Historical Ranks/Over Time/Major Categories")
WoW_Data[, `Major Category` := "Overall"]
WoW_Data <- WoW_Data[, c("Date", "ID", "Type", "Type Rank", "Major Category")] # create table of top 500 games for every day
fwrite(WoW_Data, "WoW Data Overall.csv") # write out the data

# Break
cols <- c("ID", "Rank", "WoW Rank", "Rating", "WoW Rating", "Geek Score", "WoW Geek Score", "Users Rated", "WoW Users Rated", "WoW Users Rated Percent")
i = 1
category <- major_categories[i]
all_wow_data <- WoW_Data


for(category in major_categories) {
  category_ids <- unique(major_category_list[`Major Category`==category, ID]) # filter ids to only those in major category "Thematic"
  historic_rank_category <- weekly_data.dt[ID %in% category_ids] # create table of historic data for thematic games
  
  j = 1
  set_type <- type[j]
  
  WoW_Cat_Data <- data.table()
  
  for(set_type in type) {
    primary <- paste0("WoW ", set_type)
    secondary <- if(set_type == "Geek Score") {"WoW Rating"} else {"WoW Geek Score"}
    temp = na.omit(historic_rank_category, cols = primary)
    top_n_type_increase_wow <- setorderv(temp, c(primary, secondary), c(-1, -1))[, head(.SD, n), by = Date, .SDcols = cols]
    top_n_type_increase_wow[, Type := paste0("Increase in ", set_type)]
    top_n_type_decrease_wow <- setorderv(temp, c(primary, secondary), c(1, 1))[, head(.SD, n), by = Date, .SDcols = cols]
    top_n_type_decrease_wow[, Type := paste0("Decrease in ", set_type)]
    WoW_Cat_type <- rbind(top_n_type_increase_wow, top_n_type_decrease_wow)
    rm(top_n_type_increase_wow, top_n_type_decrease_wow, temp)
    fwrite(WoW_Cat_type, paste0("WoW ", category, " ", set_type, ".csv"))
    
    WoW_Cat_Data <- rbind(WoW_Cat_Data, WoW_Cat_type)
    j = j + 1
    set_type <- type[i]
  }
  
  WoW_Cat_Data[Type %like% "Rating", Value := `WoW Rating`]
  WoW_Cat_Data[Type %like% "Rank", Value := `WoW Rank`]
  WoW_Cat_Data[Type %like% "Geek Score", Value := `WoW Geek Score`]
  WoW_Cat_Data[Type %like% "Users", Value := `WoW Users Rated`]
  WoW_Cat_Data[Type %like% "Percent", Value := `WoW Users Rated Percent`]
  
  WoW_Cat_Data[, `Type Rank` := frankv(abs(Value), order = -1, ties.method="first"), by = c("Date", "Type")]
  WoW_Cat_Data[, `Major Category` := category]
  WoW_Cat_Data <- WoW_Cat_Data[, c("Date", "ID", "Type", "Type Rank", "Major Category")]
  fwrite(WoW_Cat_Data, paste0("WoW ", category, " Data.csv"))
  
  all_wow_data <- rbind(all_wow_data, WoW_Cat_Data)
  i = i + 1
  category <- major_categories[i]
}

fwrite(all_wow_data, "WoW All Data.csv")

#------------------
# MoM By Category -
#------------------

MoM_Data[, `Major Category` := "Overall"]
MoM_Data <- MoM_Data[, c("Date", "ID", "Type", "Type Rank", "Major Category")] # create table of top 500 games for every day
fwrite(MoM_Data, "MoM Data Overall.csv") # write out the data

# Break
cols <- c("ID", "Rank", "MoM Rank", "Rating", "MoM Rating", "Geek Score", "MoM Geek Score", "Users Rated", "MoM Users Rated", "MoM Users Rated Percent")
i = 1
category <- major_categories[i]
all_MoM_data <- MoM_Data


for(category in major_categories) {
  category_ids <- unique(major_category_list[`Major Category`==category, ID]) # filter ids to only those in major category "Thematic"
  historic_rank_category <- weekly_data.dt[ID %in% category_ids] # create table of historic data for thematic games
  
  j = 1
  set_type <- type[j]
  
  MoM_Cat_Data <- data.table()
  
  for(set_type in type) {
    primary <- paste0("MoM ", set_type)
    secondary <- if(set_type == "Geek Score") {"MoM Rating"} else {"MoM Geek Score"}
    temp = na.omit(historic_rank_category, cols = primary)
    top_n_type_increase_MoM <- setorderv(temp, c(primary, secondary), c(-1, -1))[, head(.SD, n), by = Date, .SDcols = cols]
    top_n_type_increase_MoM[, Type := paste0("Increase in ", set_type)]
    top_n_type_decrease_MoM <- setorderv(temp, c(primary, secondary), c(1, 1))[, head(.SD, n), by = Date, .SDcols = cols]
    top_n_type_decrease_MoM[, Type := paste0("Decrease in ", set_type)]
    MoM_Cat_type <- rbind(top_n_type_increase_MoM, top_n_type_decrease_MoM)
    rm(top_n_type_increase_MoM, top_n_type_decrease_MoM, temp)
    fwrite(MoM_Cat_type, paste0("MoM ", category, " ", set_type, ".csv"))
    
    MoM_Cat_Data <- rbind(MoM_Cat_Data, MoM_Cat_type)
    j = j + 1
    set_type <- type[i]
  }
  
  MoM_Cat_Data[Type %like% "Rating", Value := `MoM Rating`]
  MoM_Cat_Data[Type %like% "Rank", Value := `MoM Rank`]
  MoM_Cat_Data[Type %like% "Geek Score", Value := `MoM Geek Score`]
  MoM_Cat_Data[Type %like% "Users", Value := `MoM Users Rated`]
  MoM_Cat_Data[Type %like% "Percent", Value := `MoM Users Rated Percent`]
  
  MoM_Cat_Data[, `Type Rank` := frankv(abs(Value), order = -1, ties.method="first"), by = c("Date", "Type")]
  MoM_Cat_Data[, `Major Category` := category]
  MoM_Cat_Data <- MoM_Cat_Data[, c("Date", "ID", "Type", "Type Rank", "Major Category")]
  fwrite(MoM_Cat_Data, paste0("MoM ", category, " Data.csv"))
  
  all_MoM_data <- rbind(all_MoM_data, MoM_Cat_Data)
  i = i + 1
  category <- major_categories[i]
}

fwrite(all_MoM_data, "MoM All Data.csv")
