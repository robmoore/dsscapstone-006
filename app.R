#install.packages("quanteda", "LaF", "parallel", "textcat")

# For reproducibility's sake
set.seed(19394399)

download.maybe <- function(url, refetch=FALSE, path=".") {
  dest <- file.path(path, basename(url))
  if (refetch || !file.exists(dest))
    download.file(url, dest)
  dest
}
download.maybe("http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")

# unzip file if not previously done
if (!file.exists("final")) { # final dir contains files
  unzip("Coursera-SwiftKey.zip")
}

# Download profanity list
#download.maybe("https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en")
download.maybe("https://gist.github.com/tjrobinson/2366772/raw/97329ead3d5ab06160c3c7ac1d3bcefa4f66b164/profanity.csv")
# Words to be excluded from text, eg profanity
excluded <- scan("profanity.csv", 
                 sep = "\n", 
                 what = character())
# Only keep non-dictionary words
#excluded <- setdiff(excluded, GradyAugmented) # Fuck is in grady, so need different approach
# Filter out phrases, just use words
#excluded <- excluded[grepl("^\\w+$", excluded, perl = TRUE)]

library(LaF) # for sample_lines
library(parallel)
#library(qdap)
#library(qdapRegex)
library(qdapDictionaries)
library(quanteda)
library(filehash)
library(fastmatch)

dbName <- "filehash.db"
if (!file.exists(dbName)) {
  dbCreate(dbName)
} 
db <- dbInit(dbName)

nLinesRatio <- .1 #.6
nTwitterLines <- 2360148 # can use determine_nlines("final/en_US/en_US.twitter.txt")
nBlogLines <- 899288 # can use determine_nlines("final/en_US/en_US.blogs.txt")
nNewsLines <- 1010242 # can use determine_nlines("final/en_US/en_US.news.txt")
nTotalLines <- nTwitterLines + nBlogLines + nNewsLines

db$tweets <- sample_lines("final/en_US/en_US.twitter.txt", nTwitterLines * nLinesRatio, nTwitterLines)
db$blogs <- sample_lines("final/en_US/en_US.blogs.txt", nBlogLines * nLinesRatio, nBlogLines)
db$news <- sample_lines("final/en_US/en_US.news.txt", nNewsLines * nLinesRatio, nNewsLines)

makeSentences <- function(txt) {
  txt <- unname(unlist(parSapply(cl, 
                                 txt, 
                                 function(x) tokenize(x, what = "sentence", simplify = TRUE))))
#  txt <- unname(unlist(sapply(txt, 
#                              function(x) tokenize(x, what = "sentence", simplify = TRUE))))
  txt
}

# Initiate cluster for parallel operations
cl <- makeCluster(detectCores() * .75, type="FORK") # FORK will not work on Windows

# Convert to list of sentences
db$tweetsSent <- makeSentences(db$tweets)
db$blogsSent <- makeSentences(db$blogs)
db$newsSent <- makeSentences(db$news)

# Stop cluster as we're done with it
stopCluster(cl)

# Faster lookups on vectors
"%fin%" <- function(x, table) fmatch(x, table, nomatch = 0) > 0

# Remove profanity from our dictionary
GradyAugmentedClean <- setdiff(GradyAugmented, excluded)

removeUnknown <- function(s) {
 sTokens <- tokenize(toLower(s), removePunct = TRUE, removeTwitter = TRUE, removeHyphens = TRUE)
 # UNK if not in dictionary
 sTokens <- unname(sapply(sTokens, function(x) ifelse(x %fin% GradyAugmentedClean, x, "_UNK_")))
 paste(sTokens, collapse = ' ')
}

#removeUnknownAll <- function(t) {
#  parSapply(cl, t, removeUnknown)
#}

# Initiate cluster for parallel operations
cl <- makeCluster(detectCores() * .75, type="FORK") # FORK will not work on Windows

db$tweetsClean <- parSapply(cl, db$tweetsSent, removeUnknown)
db$blogsClean <- parSapply(cl, db$blogsSent, removeUnknown)
db$newsClean <- parSapply(cl, db$newsSent, removeUnknown)
#db$tweetsClean <- sapply(db$tweetsSent, removeUnknown)

# Stop cluster as we're done with it
stopCluster(cl)



