#----------------------
# Setting up Packages -
#----------------------
library("tictoc")

#----------
# Part 1a -
#----------

tic("1a")
setwd("C:/Users/Peter.Matson/OneDrive - Calligo Limited/Onboarding/Board Game Geek Dashboard/R Scripts/master")
source("1a) BGG Historical Rank Data.R")
toc()

#----------
# Part 2a -
#----------

tic("2a")
setwd("C:/Users/Peter.Matson/OneDrive - Calligo Limited/Onboarding/Board Game Geek Dashboard/R Scripts/master")
source("2a) BGG Category Ranks Top 500.R")
toc()

#----------
# Part 2b -
#----------

tic("2b")
setwd("C:/Users/Peter.Matson/OneDrive - Calligo Limited/Onboarding/Board Game Geek Dashboard/R Scripts/master")
source("2b) BGG Category Rank + WoW MoM.R")
toc()

#---------
# Part 3 -
#---------

tic("3")
setwd("C:/Users/Peter.Matson/OneDrive - Calligo Limited/Onboarding/Board Game Geek Dashboard/R Scripts/master")
source("3) BGG Historical Data Filtered.R")
toc()

#-----------------------
# Writing out the Data -
#-----------------------

setwd("C:/Users/Peter.Matson/OneDrive - Calligo Limited/Onboarding/Board Game Geek Dashboard/Final Data")
fwrite(combined_output, "Top Plus MoM & WoW.csv") # Part 3
fwrite(historic_reduced, "Reduced Historic Data.csv") # Part 3
