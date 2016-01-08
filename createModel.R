library(parallel)
library(quanteda)
library(filehash)
library(fastmatch)
library(hash)

source("common.R")

dbName <- "corpus.db"
db <- dbInit(dbName)

tweetsSentenceCount <- length(db$tweets)
blogsSentenceCount <- length(db$blogs)
newsSentenceCount <- length(db$news)
geahSentenceCount <- length(db$geah)

createNgramsHash <- function(txt, count) {
  if (count > 1) {
    txt <- unname(parSapply(cl, txt, function(x) paste(paste(rep("_S_", count - 1), collapse = " "), 
                                                x, 
                                                paste(rep("_S_", count - 1), collapse = " "))))
  }
  # if is singleton, filter out singletons here? table(clinical.trial$age < 60)
  hash(table(tokenize(txt, ngrams = count, simplify = TRUE)))
}

createNgramsHashAll <- function(txt) {
  sapply(list(unigrams = 1, bigrams = 2, trigrams = 3), 
         function(c) createNgramsHash(txt, c), 
         USE.NAMES = TRUE)
}

#createNgramsHashAllP <- function(txt) {
#  cl <- makeCluster(detectCores() * .75)
#  r <- parSapply(cl, 
#                 list(unigrams = 1, bigrams = 2, trigrams = 3), 
#                 function(c) createNgramsHash(txt, c), 
#                 USE.NAMES = TRUE)
#  stopCluster(cl)
#}

# Make ngrams
cl <- makeCluster.default()

tweetsNgramsHash <- createNgramsHashAll(db$tweets)
blogsNgramsHash <- createNgramsHashAll(db$blogs)
newsNgramsHash <- createNgramsHashAll(db$news)
geahNgramsHash <- createNgramsHashAll(db$geah)

stopCluster(cl)

# Calculate probabilities

#length(keys(tweetsNgramsHash$unigrams))
#[1] 14853
#> sum(values(tweetsNgramsHash$unigrams))
#[1] 301433

# Create hash of percentages for unigrams, bigrams, and trigrams
createUnigramsPercentages <- function(h) {
  hash(keys(h), values(h) / sum(values(h)))
} 

createOtherGramsPercentages <- function(key, loGramPs, hiGramPs, sentenceCount) {
  loGramKey <- gsub("^(.*)_(_S_|_UNK_|[^_]+)$", "\\1", key, perl = TRUE)
  
  loGramCount <- ifelse(grepl("^((?:_)?_S_(?:_)?)*$", loGramKey),
                        sentenceCount, # Need to determine if we want to account for all sentences or all sentences + all unigram count
                        loGramPs[[loGramKey]])
  
  #print(paste("[", key, "] [", loGramKey, "]:", hiGramPs[[key]], "/", loGramCount))
  
  hiGramPs[[key]] / loGramCount
}

createNgramsPercentagesAll <- function(ngramsHash, sentenceCount) {
  list(unigrams = createUnigramsPercentages(ngramsHash$unigrams), 
       bigrams = hash(sapply(keys(ngramsHash$bigrams), function(key) createOtherGramsPercentages(key, 
                                                                                                 loGramPs = ngramsHash$unigrams, 
                                                                                                 hiGramPs = ngramsHash$bigrams, 
                                                                                                 sentenceCount = sentenceCount))),
       trigrams = hash(sapply(keys(ngramsHash$trigrams), function(key) createOtherGramsPercentages(key, 
                                                                                                  loGramPs = ngramsHash$bigrams, 
                                                                                                  hiGramPs = ngramsHash$trigrams, 
                                                                                                  sentenceCount = sentenceCount))))
}

# TODO: Remove trigrams singletons?
# invert trigrams and find values with 1 and then remove from original hash using returned values (aka keys)
# iTrigrams <- invert(tweetsNgramsHash$trigrams) # This takes a long time and isn't efficient

# Create raw percentages (frequency-based)
tweetsNgramsPercentages <- createNgramsPercentagesAll(tweetsNgramsHash, tweetsSentenceCount)
blogsNgramsPercentages <- createNgramsPercentagesAll(blogsNgramsHash, blogsSentenceCount)
newsNgramsPercentages <- createNgramsPercentagesAll(newsNgramsHash, newsSentenceCount)
geahNgramsPercentages <- createNgramsPercentagesAll(geahNgramsHash, geahSentenceCount)

# Stupid backoff lookup

#on searches:
# strip puncuation
# lowercase
# check words against dictionary (that is, change to _UNK_)

# on results back, filter out anything ending in _UNK_. that is, it's not a valid word so you'll need to backoff
