library(LaF) # for sample_lines
library(parallel)
library(qdapDictionaries)
library(quanteda)
library(filehash)
library(fastmatch)

source("common.R")

# For reproducibility's sake
set.seed(19394399)

download.maybe("http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")

# unzip file if not previously done
if (!file.exists("final")) { # final dir contains files
  unzip("Coursera-SwiftKey.zip")
}

# Download profanity list
download.maybe("https://gist.github.com/tjrobinson/2366772/raw/97329ead3d5ab06160c3c7ac1d3bcefa4f66b164/profanity.csv")
# Words to be excluded from text, eg profanity
excluded <- scan("profanity.csv", 
                 sep = "\n", 
                 what = character())

dbName <- "corpus.db"
if (!file.exists(dbName)) {
  dbCreate(dbName)
} 
db <- dbInit(dbName)

nLinesRatio <- .01 #.6
nTwitterLines <- 2360148 # can use determine_nlines("final/en_US/en_US.twitter.txt")
nBlogLines <- 899288 # can use determine_nlines("final/en_US/en_US.blogs.txt")
nNewsLines <- 1010242 # can use determine_nlines("final/en_US/en_US.news.txt")
nTotalLines <- nTwitterLines + nBlogLines + nNewsLines

db$tweets <- sample_lines("final/en_US/en_US.twitter.txt", nTwitterLines * nLinesRatio, nTwitterLines)
db$blogs <- sample_lines("final/en_US/en_US.blogs.txt", nBlogLines * nLinesRatio, nBlogLines)
db$news <- sample_lines("final/en_US/en_US.news.txt", nNewsLines * nLinesRatio, nNewsLines)

# Initiate cluster for parallel operations
cl <- makeCluster.default()

# Takes raw input and breaks out into individual sentences
makeSentences <- function(txt) {
  unlist(parSapply(cl, 
                   txt, 
                   function(x) tokenize(x, what = "sentence", simplify = TRUE)))
}

# Convert to list of sentences
db$tweets <- makeSentences(db$tweets)
db$blogs <- makeSentences(db$blogs)
db$news <- makeSentences(db$news)

# Stop cluster as we're done with it
stopCluster(cl)

#dbDelete(db, "tweets")
#dbDelete(db, "blogs")
#dbDelete(db, "news")

# Remove profanity from our dictionary
GradyAugmentedClean <- setdiff(GradyAugmented, excluded)

# Uses dictionary and profanity list to filter out potential token values
removeUnknownFromSentence <-  function(s) {
  sTokens <- tokenize(toLower(s), removePunct = TRUE, removeTwitter = TRUE, removeHyphens = TRUE)
  # UNK if not in dictionary
  sTokens <- unname(sapply(sTokens, function(x) ifelse(x %fin% GradyAugmentedClean, x, "_UNK_")))
  paste(sTokens, collapse = ' ')
}

# Initiate cluster for parallel operations
cl <- makeCluster.default()

removeUnknownFromText <- function(txt) parSapply(cl, txt, removeUnknownFromSentence)

db$tweets <- removeUnknownFromText(db$tweets)
db$blogs <- removeUnknownFromText(db$blogs)
db$news <- removeUnknownFromText(db$news)

# Stop cluster as we're done with it
stopCluster(cl)

#dbDelete(db, "tweetsSent")
#dbDelete(db, "blogsSent")
#dbDelete(db, "newsSent")

# remove stale entries
#dbReorganize(db)
#db <- dbInit(dbName)
