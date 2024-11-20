#-------------------
# Loading the Data -
#-------------------

# Installing necessary packages
library("data.table") # for efficient data manipulation
library("tm")  # for text mining
library("SnowballC") # for text stemming
library("wordcloud") # word-cloud generator 
library("RColorBrewer") # color palettes
library("tictoc") # for time tracking
# library("tidytext") # for sentiment analysis
# library("textdata")

#------------------------
# Analyzing Review Data -
#------------------------

setwd("C:/Users/v-pemats/OneDrive - Calligo Limited/Onboarding/Board Game Geek Dashboard/Raw Data")
Reviews.dt <- fread("All Reviews.csv")
Games.dt <- fread("games.csv")

beefsack <- "https://raw.githubusercontent.com/beefsack/bgg-ranking-historicals/master/" # Beefsack's github URL
beefsack_date <- as.character(Sys.Date()) # Date to be passed to the 
beefsack_api <- paste0(beefsack, beefsack_date, ".csv")

current_games.dt <- data.table(read_csv(beefsack_api))
temp <- Games.dt[!current_games.dt, on = .(ID = ID)]
Games.dt <- rbind(current_games.dt, temp)
rm(beefsack, beefsack_api, beefsack_date, current_games.dt, temp)

# Removing index column
Reviews.dt[, c("V1", "name"):=NULL]
# Reordering Columns and cleaning up column names
setcolorder(Reviews.dt, c("ID", "rating", "comment", "user"))
setnames(Reviews.dt, c("ID", "Rating", "Comment", "UserName"))
# Adding in new flags
Reviews.dt[,HasComment:=0][Comment != '', HasComment:=1]
Reviews.dt[,Kickstarted:=0][Comment %like% "Kickstarted|Kickstarter" | Comment == 'KS', Kickstarted:=1]

# Splitting reviews based on comment status
Reviews.NC.dt <- Reviews.dt[HasComment==0]
Reviews.dt <- Reviews.dt[HasComment==1]

# Replacing BBCode image with board game name (only for current reference and not for all references)
i = 1
Reviews.Out.dt <- data.table()
Reviews.ID <- unique(Reviews.dt[, ID])
n = Reviews.ID[i] 

for(i in 1:length(Reviews.ID)) {
  Replacement = paste0("\\[thing=", n, "\\]")
  Name = Games.dt[ID == n, Name]
  temp <- Reviews.dt[ID == n,]
  temp[, Comment:=gsub(Replacement, Name, Comment)]
  Reviews.Out.dt <- rbind(Reviews.Out.dt, temp)
  i = i+1
  n = Reviews.ID[i] 
}
Reviews.dt <- Reviews.Out.dt

# Removing BBCode
Reviews.dt[, Comment:=gsub("\\[\\S+\\]", "", Comment)]
# Removing star, halfstar, and nostar emojis
Reviews.dt[HasComment == 1, Comment:=gsub("\\:star\\:", "+", Comment)]
Reviews.dt[HasComment == 1, Comment:=gsub("\\:halfstar\\:", "*", Comment)]
Reviews.dt[HasComment == 1, Comment:=gsub("\\:nostar\\:", "-", Comment)]
# Removing remaining emojis
Reviews.dt[HasComment == 1, Comment:=gsub("\\:\\S+\\:", "", Comment)]
# Replacing iterations of double quotes ("") with single double quotes(")
Reviews.dt[HasComment == 1, Comment:=gsub('""""', '"', Comment)]
Reviews.dt[HasComment == 1, Comment:=gsub('"""', '"', Comment)]
Reviews.dt[HasComment == 1, Comment:=gsub('""', '"', Comment)]

# # Detecting comment language
# Reviews.dt[, Lang:=detect_language(Comment)]
# Reviews.NC.dt[, Lang:=NA]

# Recombining Reviews with Comments to those without
Reviews.dt <- rbind(Reviews.dt, Reviews.NC.dt)

#---------
# Part 2 -
#---------

# Reading in the data
setwd("C:/Users/v-pemats/OneDrive - Calligo Limited/Onboarding/Board Game Geek Dashboard/Prepped Data")
#Reviews.dt <- fread("Reviews.csv")
fwrite(Reviews.dt, "Reviews.csv")

# Adding board game name to the Reviews and removing ratings without ratings
Data.dt <- Reviews.dt[Comment != "" & !is.na(Comment),][Games.dt[, c("ID", "Name")], on = "ID", nomatch = 0]

# Removing now unneeded data
rm(Games.dt, Reviews.dt)

#-------------------------
# Setting Up Text Mining -
#-------------------------

# adding the words in the title to words to remove
words_to_remove <- c("game", "games", "play", "played", "the", "board", "one", "can", "still", "get",
                     "this", "just", "will", "its", "got", "lot", "now", "player", "playing", "players", "many",
                     "always", "ive", "times", "never", "bit", "plays", "also", "though", "every",
                     "gamers", "but", "really", "dont", "even", "like", "much", "well", "way", "make", "opinion",
                     "enough", "version", "quite", "another", "end", "actually", "see", "need", "box", "set",
                     "take", "around", "doesnt", "based", "seems", "didnt", "right", "cant", "find", "want",
                     "makes", "que", "ever", "almost", "know", "try", "add", "bought", "bought", "use", "lots", "copy",
                     "idea", "may", "buy", "probably", "might", "although", "less", "going", "nothing", "away", "made",
                     "feels", "gets", "number", "theres", "however", "something", "rather", "give", "anything", "juego",
                     "adds", "definitely", "things", "sure", "table", "able", "real", "rule", "year", "years", "amount",
                     "since", "last", "base", "keep", "come", "found", "short", "either", "basically", "isnt", "place",
                     "ago", "without", "bits", "addition", "trying", "looking", "added", "least", "used", "especially",
                     "part", "aspect", "anyone", "para", "already", "look", "push", "words", "youre", "los", "early", "along",
                     "taking", "day", "similar", "takes", "comes", "change", "arent", "jeu", "instead", "done", "asi")

# setting board game ID
final <- NULL
ids <- unique(Data.dt$ID)

f100_ids <- ids[1:1000]
i = f100_ids[1]
n = 1
#--------------------
# Cleaning The Text -
#--------------------
tic("next 1000")
for (i in ids) {

# Creating a Corpus to work with 
Reviews_TA <- VCorpus(VectorSource(Data.dt[ID == i, Comment]))
# getting board game name 
name <- unique(Data.dt[ID == i, `Name`])
name_words <- unlist(strsplit(tolower(name), " "))
name_words <- gsub("[^[:alnum:]]", "", name_words)

Reviews_TA <- tm_map(Reviews_TA, content_transformer(tolower)) # Convert the text to lower case
Reviews_TA <- tm_map(Reviews_TA, removePunctuation) # Remove punctuations
Reviews_TA <- tm_map(Reviews_TA, stripWhitespace) # Eliminate extra white spaces
Reviews_TA <- tm_map(Reviews_TA, removeWords, stopwords("english")) # Remove english common stopwords

# Remove your own stop word
# specify your stopwords as a character vector
Reviews_TA <- tm_map(Reviews_TA, removeWords, c(words_to_remove, name_words, new_removal))

dtm <- TermDocumentMatrix(Reviews_TA)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.table(word = names(v),freq=v)

d <- head(d[, ID := i], 30)
final <- rbind(final, d)

i = ids[n+1]
}
toc()
fwrite(final, "Top Words.csv")

#----------------------
# Additional Analysis -
#----------------------

# Reading in the data
setwd("C:/Users/peter.matson/OneDrive - Calligo Limited/Onboarding/Board Game Geek Dashboard/Prepped Data")
TopWords.dt <- fread("Top Words.csv")

TopWords.dt[, `:=`(count=.N, amount=sum(freq)), by = word]
Duplicates <- unique(TopWords.dt[count>1,c("word", "count", "amount")])
Duplicates[, check:=(amount/count)]                     

new_removal <- Duplicates[count > 200 & 
                            check < 5 & 
                            !(word %in% c("awesome", "boring", "excellent", "challenging", "big",
                                          "amazing", "adult", "decent", "challenge", "essen")), word]

Reviews.dt <- fread("Reviews.csv")
Reviews.dt[, Round_Rating := round(Rating, 0)]
dim_reviews <- Reviews.dt[, .(number = .N, commented = sum(HasComment), ks = sum(Kickstarted)), by = c("ID", "Round_Rating")]
dim_reviews[, `:=`(pure_rating = number - commented, pure_comment = commented - ks)]
setnames(dim_reviews, c("ID", "Rating", "Total Ratings", "Comments", "Kickstarter", "Rating Only", "Comment Only"))
fwrite(dim_reviews, "Dim Reviews.csv")

Comments <- Reviews.dt[HasComment == 1]
Comments[, comment_length := nchar(Comment)]
# Comments[, comment_rank := frank(comment_length, ties.method = "first"), by = c("ID","Round_Rating")]
Comments.Reduced <- setorderv(Comments, c("ID", "Round_Rating", "comment_length"), c(1, 1, -1))[, head(.SD, 10), by = c("ID", "Round_Rating")]
Comments.Reduced[, c("Rating", "HasComment", "Kickstarted") := NULL]

fwrite(Comments.Reduced, "Reduced Comments.csv")
