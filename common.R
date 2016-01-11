library(LaF) # for sample_lines
library(parallel)
library(qdapDictionaries)
library(quanteda)
library(filehash)
library(fastmatch)
library(pipeR)
library(rlist)
library(hash)

filehashOption(defaultType = "RDS")
dbName <- "corpus"

download.maybe <- function(url, refetch=FALSE, path=".") {
  dest <- file.path(path, basename(url))
  if (refetch || !file.exists(dest))
    download.file(url, dest)
  dest
}

# Faster lookups on vectors
"%fin%" <- function(x, table) fmatch(x, table, nomatch = 0) > 0

#makeCluster.default <- function() makeCluster(detectCores() * .75, type="FORK") # FORK will not work on Windows
makeCluster.default <- function() makeForkCluster(detectCores() * .75)

# Takes raw input and breaks out into individual sentences
makeSentences <- function(txt) {
  unlist(parSapply(cl, 
                   txt, 
                   function(x) tokenize(x, what = "sentence", simplify = TRUE)))
}

# Uses dictionary and profanity list to filter out potential token values
removeUnknownFromSentence <-  function(s) {
  sTokens <- tokenize(toLower(s), removePunct = TRUE, removeTwitter = TRUE, removeHyphens = TRUE)
  # UNK if not in dictionary
  sTokens <- unname(sapply(sTokens, function(x) ifelse(x %fin% GradyAugmentedClean, x, "_UNK_")))
  paste(sTokens, collapse = ' ')
}

removeUnknownFromText <- function(txt) parSapply(cl, txt, removeUnknownFromSentence)

rec <- function(l, s = "") {
  #print(s)
  ifelse(length(l) != 0, 
         rec(l[1:length(l) - 1], paste0(s, l[length(l)], " ")), 
         trimws(s))
}

grabFiles <- function() {
  download.maybe("http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")
  
  # unzip file if not previously done
  if (!file.exists("final")) { # final dir contains files
    unzip("Coursera-SwiftKey.zip")
  }
  
  # Download profanity list
  download.maybe("https://gist.github.com/tjrobinson/2366772/raw/97329ead3d5ab06160c3c7ac1d3bcefa4f66b164/profanity.csv")
}

#---

createNgramsHash <- function(txt, count, removeSingletons = TRUE) {
  if (count > 1) {
    txt <- sapply(txt, function(x) paste(paste(rep("_S_", count - 1), collapse = " "), 
                                         x, 
                                         paste(rep("_S_", count - 1), collapse = " ")))
  }
  
  t <- table(tokenize(txt, ngrams = count, simplify = TRUE))
  # filter out singletons for count > 2
  if (count > 2 && removeSingletons) t <- t[t > 1]
  hash(t)
}

createNgramsHashAll <- function(txt, removeSingletons = TRUE) {
  parSapply(cl, list(unigrams = 1, bigrams = 2, trigrams = 3), 
            function(c) createNgramsHash(txt, c, removeSingletons))
}

# Calculate probabilities

# Create hash of percentages for unigrams, bigrams, and trigrams
createUnigramsPercentages <- function(h) {
  hash(keys(h), values(h) / sum(values(h)))
} 

createOtherGramsPercentages <- function(key, loGramPs, hiGramPs, sentenceCount) {
  loGramKey <- gsub("^(.*)_(_S_|_UNK_|[^_]+)$", "\\1", key, perl = TRUE)
  
  loGramCount <- if(grepl("^((?:_)?_S_(?:_)?)*$", loGramKey))
    sentenceCount # Need to determine if we want to account for all sentences or all sentences + all unigram count
  else
    loGramPs[[loGramKey]]
  
  #print(paste("[", key, "] [", loGramKey, "]:", hiGramPs[[key]], "/", loGramCount))
  
  hiGramPs[[key]] / loGramCount
}

createNgramsPercentagesAll <- function(ngramsHash, sentenceCount) {
  list(unigrams = createUnigramsPercentages(ngramsHash$unigrams), 
       bigrams = hash(parSapply(cl, keys(ngramsHash$bigrams), function(key) createOtherGramsPercentages(key, 
                                                                                                        loGramPs = ngramsHash$unigrams, 
                                                                                                        hiGramPs = ngramsHash$bigrams, 
                                                                                                        sentenceCount = sentenceCount))),
       trigrams = hash(parSapply(cl, keys(ngramsHash$trigrams), function(key) createOtherGramsPercentages(key, 
                                                                                                          loGramPs = ngramsHash$bigrams, 
                                                                                                          hiGramPs = ngramsHash$trigrams, 
                                                                                                          sentenceCount = sentenceCount))))
}

# only keeps last tokens on an ngram (_ sep) string
ngramTail <- function(ngram, count = ngramLength(ngram) - 1) {
  q <- unlist(strsplit(ngram, "_", fixed = TRUE))
  paste(tail(q, count), collapse = "_")
}

ngramLength <- function(ngram) {
  length(unlist(strsplit(ngram, "_", fixed = TRUE)))
}

# adjust ngram down to length of ngram in query + 1 before making this call
matchingKeys <- function(ngram, ngrams) {
  nl <- ngramLength(ngram)
  keys(ngrams[[nl + 1]])[grepl(paste0("^", ngram, "_"), 
                               keys(ngrams[[nl + 1]]))]
}

matchingValues <- function(ngram, ngrams, n = 3) {
  nl <- ngramLength(ngram)
  vs <- as.list(ngrams[[nl + 1]][matchingKeys(ngram, ngrams)])
  #print(paste0("ngram:", ngram, ", vs:", vs))
  vs %>>% list.filter(!grepl('(_S_|_UNK_)$',.name)) %>>% list.sort(-max(.)) %>>% list.take(n)
}

# Note: Stupid backoff applied here
processValues <- function(ngram, ngrams, n) {
  vs <- matchingValues(ngram, ngrams)
  #print(vs)
  if(length(vs) != 0) 
    hash(sapply(vs,  
                function(x) if (n == 0) x else x * n * .4))
  else
    hash()
}

countResults <- function(rs) {
  str(rs)
  if(length(rs) != 0) 
    sum(sapply(rs, function(r) if(length(r) != 0) sapply(r, function(sr) length(sr)) else 0))
  else 0
}

# TODO: if single ngram (unigram) use rep _S_ at beginning to fill it out?
queryNgramProbs <- function(ngram, ngrams, ks = c(), vs = c(), n = 0) {
  #print(paste("ks:", ks))
  #print(paste("count for", ks, ":", countResults(vs)))
  nl <- ngramLength(ngram)
  nsl <- length(ngrams)
  if (nsl <= nl) # If too long for # of ngram lengths we have then trim down
    Recall(ngram = ngramTail(ngram, nsl - 1),
           ngrams = ngrams)
  else if(nl != 0 && countResults(vs) < 3)
    Recall(ngram = ngramTail(ngram), 
           ngrams = ngrams, 
           ks = c(ks, ngram),
           vs = c(vs, processValues(ngram, ngrams, n)),
           n = n + 1)
  else 
    hash(ks, vs)
}

cleanQuery <- function(query, maxCount) {
  tQuery <- unname(unlist(tokenize(query)))
  if(!missing(maxCount) && maxCount < length(tQuery))
    tQuery <- tail(tQuery, maxCount)
  paste(tQuery, collapse="_")
}