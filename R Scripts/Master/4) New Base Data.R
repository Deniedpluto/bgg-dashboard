# What does this Script do?

# This takes all the ids and pulls them in from the BGG API

# inputs
# MoM All Data and WoW All Data
# top_500_all_categories_reduced

# Output 
# historic_data_reduced (read in to Power BI)
# Top Plus MoM & WoW (read in to Power BI)

#-------------------------------
# Loading in required packages -
#-------------------------------

# Installing necessary packages
library("data.table")
library("httr")
library("XML")
#library("xml2")
library("readr")

wait <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1
}

#-----------------------
# Creating List of Ids -
#-----------------------

setwd("C:/Users/Peter.Matson/OneDrive - Calligo Limited/Onboarding/Board Game Geek Dashboard/Historical Ranks/Major Categories") # setting the working directory
all_ids <- unique(fread("beefsack_ids.csv")[, all_ids]) # reading in list of all historic ids

#----------------------------
# Loading Data from BGG API -
#----------------------------

setwd("C:/Users/Peter.Matson/OneDrive - Calligo Limited/Onboarding/Board Game Geek Dashboard/Raw Data/XML New") # setting the working directory

iterations = ceiling(length(all_ids)/20) - 1 # create a number of iterations based on groups of 500 ids
i = 0 # setting the start value at 0 (since 0 * 500 + 1 = 1)

bgg1 <- "https://www.boardgamegeek.com/xmlapi2/thing?id=" # getting the first part of the bgg xmlapi2 url
bgg2 <- "&stats=1" # setting to bgg xmlapi2 to retrieve stats (for major category)

for(i in 0:iterations) {

  pull_id_list <- all_ids[((i*20)+1):((i+1)*20)] # reducing the list to 20 (or less) ids
  pull_id_list <- pull_id_list[!sapply(pull_id_list, is.na)] # removing NAs from the list 
  active_ids <- paste0(pull_id_list, sep = ",", collapse = "") # converting the list to text
  active_ids <- substr(active_ids, 1, nchar(active_ids)-1) # removing dangling comma
  bgg_api <- paste0(bgg1, active_ids, bgg2) # setting 500 ids inside the bgg xmlapi2 call
  
  max_attempts = 3
  attempt_count <- 0
  success <- FALSE
    
  while(!success && attempt_count < max_attempts) {
      attempt_count <- attempt_count + 1
      
      tryCatch({
        bgg_data <- xmlParse(GET(bgg_api)) # reading in the xml data
        success <- TRUE # If the query succeeds, set success to TRUE
      }, 
      error = function(e) {
        print(paste0("Reading file ", i, ": Attempt", attempt_count, " failed."))
        wait(10)  # Wait for a specified time before retrying
      })
    }
 
  
  saveXML(bgg_data, paste0("XMLData_", i+1, ".xml")) # save out the xml data
  
  wait(7) # wait 5 seconds
  i = i + 1  # increase i to next iteration
  print(paste0("Written files ", i, " of ", iterations+1))
}
rm(bgg_data) # remove duplicate temp data

#-----------------------
# Combine all XML Data -
#-----------------------

file_list <- data.table(files = list.files("C:/Users/Peter.Matson/OneDrive - Calligo Limited/Onboarding/Board Game Geek Dashboard/Raw Data/XML New", pattern = ".xml")) # creating a list of all xml's in the directory

doc = newXMLDoc()
root = newXMLNode("root", doc = doc)

n <- 1

for (n in 1:nrow(file_list)) {
  temp <- xmlParse(paste0("XMLData_", n, ".xml"))
  # APPEND FROM API XML ROOT
  root <- addChildren(root, getNodeSet(temp, '/items/item'))
  n <- n + 1
}
saveXML(root, "Compiled.xml") # save out the xml data
