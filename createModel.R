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

stopCluster(cl)

# Calculate probabilities

#length(keys(tweetsNgramsHash$unigrams))
#[1] 14853
#> sum(values(tweetsNgramsHash$unigrams))
#[1] 301433

# Create hash of percentages for unigrams, bigrams, and trigrams
createNgramsPercentages <- function(h) {
  hash(keys(h), values(h) / sum(values(h)))
}

#createNgramsPercentages <- function(h) hash(keys(h), values(h) / sum(values(h)))

createNgramsPercentagesAll <- function(l) {
  sapply(list(unigrams = "unigrams", bigrams = "bigrams", trigrams = "trigrams"), 
         function(n) createNgramsPercentages(l[[n]]), 
         USE.NAMES = TRUE)
}

# Remove trigrams singletons?
# invert trigrams and find values with 1 and then remove from original hash using returned values (aka keys)
# iTrigrams <- invert(tweetsNgramsHash$trigrams) # This takes a long time and isn't efficient

# Create raw percentages (frequency-based)
tweetsNgramsPercentages <- createNgramsPercentagesAll(tweetsNgramsHash)
blogsNgramsPercentages <- createNgramsPercentagesAll(blogsNgramsHash)
newsNgramsPercentages <- createNgramsPercentagesAll(newsNgramsHash)

# Create normalized percentages (based on n-1 gram percentages)

# For each bigram or trigram
# get values, get first component (eg, a of a_b) and look up percantage of that

createNormalizedPercentages <- function(key, loGramPs, hiGramPs, sentenceCount) {
  #loGramKey <- gsub("^(_S_|_UNK_|[^_]*)_(?:_S_|_UNK_|[^_]*)$", "\\1", key, perl = TRUE)
  loGramKey <- gsub("^(_S_|_UNK_|[^_]*)(?:_(_S_|_UNK_|[^_]*))$|^((_S_|_UNK_|[^_]*)_(_S_|_UNK_|[^_]*))(?:_(_S_|_UNK_|[^_]*))$", 
                    "\\1\\3", key, perl = TRUE)

  #print(loGramKey)

  loGramCount <- ifelse(identical(loGramKey, "_S_") || identical(loGramKey, "_S___S_"),
                        sentenceCount, # Need to determine if we want to account for all sentences or all sentences + all unigram count
                        loGramPs[[loGramKey]])

  hiGramPs[[key]] / loGramCount
}

createNgramsNormPercentages <- function(ngramsPercentages, sentenceCount) { # should sentence count be total # of items in ngram list?
  list(bigrams = hash(sapply(keys(ngramsPercentages$bigrams), function(key) createNormalizedPercentages(key, 
                                                                                                   loGramPs = ngramsPercentages$unigrams, 
                                                                                                   hiGramPs = ngramsPercentages$bigrams, 
                                                                                                  sentenceCount = sentenceCount))),
       trigrams = hash(sapply(keys(ngramsPercentages$trigrams), function(key) createNormalizedPercentages(key, 
                                                                                                     loGramPs = ngramsPercentages$bigrams, 
                                                                                                     hiGramPs = ngramsPercentages$trigrams, 
                                                                                                     sentenceCount = sentenceCount))))
}

tweetsNgramsNormPercentages <- createNgramsNormPercentages(ngramsPercentages = tweetsNgramsPercentages,
                                                           sentenceCount = tweetsSentenceCount)

blogsNgramsNormPercentages <- createNgramsNormPercentages(ngramsPercentages = blogsNgramsPercentages,
                                                           sentenceCount = blogsSentenceCount)

newsNgramsNormPercentages <- createNgramsNormPercentages(ngramsPercentages = newsNgramsPercentages,
                                                           sentenceCount = newsSentenceCount)


# Stupid backoff lookup

#on searches:
# strip puncuation
# lowercase
# check words against dictionary (that is, change to _UNK_)

# on results back, filter out anything ending in _UNK_. that is, it's not a valid word so you'll need to backoff
