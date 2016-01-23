library(LaF) # for sample_lines
library(parallel)
library(qdapDictionaries)
library(quanteda)
library(filehash)
library(fastmatch)
library(pipeR)
library(rlist)
library(hash)
library(stringi)
library(stringr)
# look at stashR for remote repo

# For reproducibility's sake
set.seed(19394399)

# Words to be excluded from text, eg profanity
excluded <- scan("profanity.csv", 
                 sep = "\n", 
                 what = character())

# Remove profanity from our dictionary
GradyAugmentedClean <- setdiff(GradyAugmented, excluded)
rm(excluded)

dbName <- "corpus.db1"
cleanDbName <- "cleaned.db1"
modelDbName <- "model.db1"

download.maybe <- function(url, refetch=FALSE, path=".") {
  dest <- file.path(path, basename(url))
  if (refetch || !file.exists(dest))
    download.file(url, dest)
  dest
}

# Faster lookups on vectors
"%fin%" <- function(x, table) fmatch(x, table, nomatch = 0) > 0

makeCluster.default <- function() makeForkCluster(detectCores() * .75, outfile = "cluster-outfile.txt")
coreCount <- detectCores() * .75

# Takes raw input and breaks out into individual sentences
makeSentences <- function(txt) {
  print("Making sentences")
  unlist(mclapply2(txt, 
                   function(x) tokenize(x, what = "sentence", simplify = TRUE), mc.cores = coreCount))
}

# Uses dictionary and profanity list to filter out potential token values
removeUnknownFromSentence <-  function(s) {
  if (nchar(s) != 0) {
    sTokens <- tokenize(toLower(s), removePunct = TRUE, removeTwitter = TRUE, removeHyphens = TRUE)
    # UNK if not in dictionary
    sTokens <- sapply(sTokens, function(x) ifelse(x %fin% GradyAugmentedClean, x, "_UNK_"))
    sTokens <- sapply(sTokens, function(x) gsub("http[^ ]*", "_UNK_", x))
    paste(sTokens, collapse = ' ')
  } else {
    s
  }
}

removeUnknownFromText <- function(txt) {
  print("Removing unknown from text")
  mclapply2(txt, removeUnknownFromSentence, mc.cores = coreCount)
}

grabFiles <- function() {
  download.maybe("http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")
  
  # unzip file if not previously done
  if (!file.exists("final")) { # final dir contains files
    unzip("Coursera-SwiftKey.zip")
  }
  
  # Download profanity list
  download.maybe("https://gist.github.com/tjrobinson/2366772/raw/97329ead3d5ab06160c3c7ac1d3bcefa4f66b164/profanity.csv")

  # capstone4
  download.maybe("https://github.com/hfoffani/dsci-benchmark/raw/master/data.zip")
}

grabFiles()

#---

# TODO: Do we really need the parSapply or even sapply below? it must not be a list or the tokennize call wouldn't work, right?
createNgramsHash <- function(txt, count, removeSingletons = TRUE) {
  if (count > 1)
    #txt <- parSapply(cl, txt, function(x) paste(paste(rep("_S_", count - 1), collapse = " "), x, paste(rep("_S_", count - 1), collapse = " ")))
    txt <- mclapply(txt, function(x) paste(paste(rep("_S_", count - 1), collapse = " "), x, paste(rep("_S_", count - 1), collapse = " ")), mc.cores = coreCount)

  t <- table(tokenize(unlist(txt), ngrams = count, simplify = TRUE))
  # filter out singletons for count > 2
  if (count > 2 && removeSingletons) t <- t[t > 1]
  hash(t)
}

createNgramsHashAll <- function(txt, removeSingletons = TRUE) {
  print("Creating ngram hashes")
  mclapply2(list(unigrams = 1, bigrams = 2, trigrams = 3, quadgrams = 4), 
            function(c) createNgramsHash(txt, c, removeSingletons && c > 2), mc.cores = coreCount)
#  sapply(list(unigrams = 1, bigrams = 2, trigrams = 3, quadgrams = 4), 
#            function(c) createNgramsHash(txt, c, removeSingletons && c > 2))
}

# Create hash of percentages for unigrams, bigrams, and trigrams
createUnigramsPercentages <- function(h) {
  #print(paste("Calculating percentages for all unigrams"))
  hash(keys(h), values(h) / sum(values(h)))
} 

createOtherGramsPercentages <- function(key, loGramPs, hiGramPs, sentenceCount) {
  #print(paste("Calculating percentages for", key))
  loGramKey <- gsub("^(.*)_(_S_|_UNK_|[^_]+)$", "\\1", key, perl = TRUE)
  
  loGramCount <- if(grepl("^((?:_)?_S_(?:_)?)*$", loGramKey, perl = TRUE))
    sentenceCount # Need to determine if we want to account for all sentences or all sentences + all unigram count
  else
    loGramPs[[loGramKey]]
  
  #print(paste("[", key, "] [", loGramKey, "]:", hiGramPs[[key]], "/", loGramCount))
  
  hiGramPs[[key]] / loGramCount
}

createNgramsPercentagesAll <- function(ngramsHash, sentenceCount) {
  print("Creating ngram percentages")
  list(unigrams = createUnigramsPercentages(ngramsHash$unigrams), 
       bigrams = hash(mcmapply2(function(key) createOtherGramsPercentages(key, 
                                                                         loGramPs = ngramsHash$unigrams, 
                                                                         hiGramPs = ngramsHash$bigrams, 
                                                                         sentenceCount = sentenceCount), keys(ngramsHash$bigrams), SIMPLIFY = TRUE, mc.cores = coreCount)),
       trigrams = hash(mcmapply2(function(key) createOtherGramsPercentages(key, 
                                                                          loGramPs = ngramsHash$bigrams,
                                                                          hiGramPs = ngramsHash$trigrams, 
                                                                          sentenceCount = sentenceCount), keys(ngramsHash$trigrams), SIMPLIFY = TRUE, mc.cores = coreCount)),
       quadgrams = hash(mcmapply(function(key) createOtherGramsPercentages(key, 
                                                                           loGramPs = ngramsHash$trigrams, 
                                                                           hiGramPs = ngramsHash$quadgrams, 
                                                                           sentenceCount = sentenceCount), keys(ngramsHash$quadgrams), SIMPLIFY = TRUE, mc.cores = coreCount)))
}

# only keeps last tokens on an ngram (_ sep) string
ngramTail <- function(ngram, count = ngramLength(ngram) - 1) {
  q <- stri_match_all_regex(ngram, "(_S_|_UNK_|[^_]+)")[[1]][,1]
  paste(tail(q, count), collapse = "_")
}

ngramHead <- function(ngram, count = 1) {
  q <- stri_match_all_regex(ngram, "(_S_|_UNK_|[^_]+)")[[1]][,1]
  paste(head(q, count), collapse = "_")
}

# Returns indicated token/tokens using index
# ngramToken <- function(ngram, count = 1) {
#   stri_match_all_regex(ngram, "(_S_|_UNK_|[^_]+)")[[1]][,1][count]
# }

ngramLength <- function(ngram) {
  #str(ngram)
  stri_count_regex(ngram, "(_S_|_UNK_|[^_]+)")
}

# # adjust ngram down to length of ngram in query + 1 before making this call
# matchingKeys <- function(ngram, ngrams) {
#   nl <- ngramLength(ngram)
#   ks <- keys(ngrams[[nl + 1]])
#   ks[grepl(paste0("^", ngram, "_"), # Use fastmatch here?
#            ks)]
# }
# 
# matchingValues <- function(ngram, ngrams, n = 3) {
#   nl <- ngramLength(ngram)
#   vs <- as.list(ngrams[[nl + 1]][matchingKeys(ngram, ngrams)])
#   #print(paste0("ngram:", ngram, ", vs:", vs))
#   vs %>>% list.filter(!grepl('(_S_|_UNK_)$',.name)) %>>% list.sort(-max(.)) %>>% list.take(n)
# }
# 
# # Note: Stupid backoff applied here
# processValues <- function(ngram, ngrams, n) {
#   vs <- matchingValues(ngram, ngrams)
#   #print(vs)
#   if(length(vs) != 0) 
#     hash(sapply(vs,  
#                 function(x) if (n == 0) x else x * n * .4))
#   else
#     hash()
# }

# countResults <- function(rs) {
#   #str(rs)
#   if(length(rs) != 0) 
#     length(list.cases(sapply(list.cases(sapply(rs, function(r) keys(r))), function(x) ngramTail(x,1))))
#   else 0
# }
# 
# queryNgramProbs <- function(ngram, ngrams, ks = c(), vs = c(), n = 0, maxResults = 3) {
#   nl <- ngramLength(ngram)
#   nsl <- length(ngrams)
#   if (nsl <= nl) # If too long for # of ngram lengths we have then trim down
#     Recall(ngram = ngramTail(ngram, nsl - 1),
#            ngrams = ngrams)
#   else if(nl != 0 && countResults(vs) < maxResults)
#     Recall(ngram = ngramTail(ngram), 
#            ngrams = ngrams, 
#            ks = c(ks, ngram),
#            vs = c(vs, processValues(ngram, ngrams, n)),
#            n = n + 1)
#   else 
#     hash(ks, vs)
# }

cleanQuery <- function(query, maxCount) {
  #print(query)
  if (nchar(query) != 0) {
    tQuery <- unname(unlist(tokenize(query)))
    if(!missing(maxCount) && maxCount < length(tQuery))
      tQuery <- tail(tQuery, maxCount)
    paste(tQuery, collapse="_")
  } else {
    query
  }
}

# Create percentages (frequency-based)
calculatePercentages <- function(txtV, removeSingletons = TRUE) 
  createNgramsPercentagesAll(createNgramsHashAll(txtV, removeSingletons), length(txtV))

quotemeta <- function(string) {
  str_replace_all(string, "(\\W)", "\\\\\\1")
}

# TODO: Consider storing the name of the subnode in the node when creating the maps to begin with?
makeModel <- function(ngram, ngrams, subKeys, count = 1) {
  #print(paste("Processing", ngram))
  ngramsLength = length(ngrams)
  p = ngrams[[count]][[ngram]] 
  sbMultiplier <- ngramsLength - count
  if (sbMultiplier != 0)
    p <- p * sbMultiplier * .4
  node <- list(ngram = ngram, p = p)
  if (count != ngramsLength) {
    subnodes <- grep(paste0("^", quotemeta(ngram), "_"), subKeys, value = TRUE, perl = TRUE)
    if (length(subnodes) != 0) {
      node$snodeProbs <- mclapply(subnodes, 
                                  function(subnode) list(w = ngramTail(subnode, 1),
                                                         p = (ngrams[[count + 1]][[subnode]]))) %>>%  list.sort((p))
    }
  }
  node
}

makeModel2 <- function(ngram, ngrams, ngramsMap, count = 1) {
  #print(paste("Processing", ngram))
  ngramsLength = length(ngrams)
  p = ngrams[[count]][[ngram]] 
  sbMultiplier <- ngramsLength - count
  if (sbMultiplier != 0)
    p <- p * sbMultiplier * .4
  node <- list(ngram = ngram, p = p)
  if (count != ngramsLength) {
    subnodes <- ngramsMap[[count]][[ngram]]
    if (length(subnodes) != 0) {
      node$snodeProbs <- mclapply(subnodes, 
                                  function(subnode) list(w = ngramTail(subnode, 1),
                                                         p = (ngrams[[count + 1]][[subnode]]))) %>>%  list.sort((p))
    }
  }
  node
}

makeNgramModelWrapper <- function(ngrams, count = 1) {
  hash(mcmapply2(function(ngram) makeModel(ngram, ngrams, keys(ngrams[[count + 1]]), count), 
                 keys(ngrams[[count]]), 
                 SIMPLIFY = FALSE, 
                 mc.cores = coreCount))
}

makeNgramModelWrapper2<- function(ngrams, ngramsMap, count = 1) {
  print("Creating ngram model")
  hash(mcmapply2(function(ngram) makeModel2(ngram, ngrams, ngramsMap, count), 
                 keys(ngrams[[count]]), 
                 SIMPLIFY = FALSE, 
                 mc.cores = coreCount))
}

makeNgramModel <- function(ngrams) {
  list(unigrams = makeNgramModelWrapper(ngrams, 1),
       bigrams = makeNgramModelWrapper(ngrams, 2),
       trigrams = makeNgramModelWrapper(ngrams, 3),
       quadgrams = ngrams$quadgrams)
}

makeNgramModel2 <- function(ngrams, ngramsMap) {
  list(unigrams = makeNgramModelWrapper2(ngrams, ngramsMap, 1),
       bigrams = makeNgramModelWrapper2(ngrams, ngramsMap, 2),
       trigrams = makeNgramModelWrapper2(ngrams, ngramsMap, 3),
       quadgrams = ngrams$quadgrams)
}

mapGrams <- function(h, ngram, ngrams, count = ngramLength(ngram)) {
  #print(ngram)
  pn <- ngramHead(ngram, count - 1)
  # this has got to be single threaded or it could clobber other entries
  snList <- h[[pn]]
  h[[pn]] <- if (is.null(snList)) list(ngram) else c(snList, list(ngram))
}

mapGramsWrapper <- function(ngrams) {
  print("Creating mapGrams")
  x <- mcmapply2(function(x) {
    h <- hash()
    lapply(keys(ngrams[[x]]), function(y) mapGrams(h, y, ngrams))
    h
  }, names(ngrams[-1]))
  names(x) <- names(ngrams)[-length(ngrams)]
  x
}

lookupProbs <- function(ngram, ngrams, resultsCount = 0) {
  ngramLength <- ngramLength(ngram)
  #print(ngramLength)
  node <- ngrams[[ngramLength]][[ngram]] 
  #print(node)
  results <- node$snodeProbs[1:3] %>>% list.clean
  resultsCount <- resultsCount + length(results)
  #print(resultsCount)
  
  if (ngramLength > 1 && resultsCount < 3)
    results <- c(results, lookupProbs(ngramTail(ngram), ngrams, resultsCount))

  results
}

lookupProbsWrapper <- function(ngram, ngrams) {
  #str(ngram)
  results <- lookupProbs(ngram, ngrams)
  #str(results)
  results %>>% list.sort(-p) %>>% list.group(w) %>>% list.map(list.first(.)) %>>% list.take(3) %>>% list.sort(-p) %>>% unname
}

predict.baseline.raw <- function(q, models) {
  print(paste0("Predicting for '", q, "'"))
  if (nchar(trimws(q)) == 0) {
    q <- paste(replicate(3, "_S_"), collapse = "_")
  } else {
    q <- removeUnknownFromSentence(q)
    q <- cleanQuery(q, 3)
    # Fill out query to 3 tokens if less
    tokenCount <- stri_count_regex(q, "([^_]+)")
    if (tokenCount < 3)
      q <- paste0(paste0(replicate(3 - tokenCount, "_S_"), collapse = "_"), "_", q)
  }
  print(paste("Modified query:", q))
  
  #TODO: filter out duplicates here and order across all
  suggestions <- c(lapply(models, function(model) lookupProbsWrapper(q, model)))
  print(paste("Suggestions:", paste(suggestions, collapse = ",")))
  suggestions %>>% list.mapv(w) #%>>% list.flatten(use.names = FALSE)
}

##------------------------------------------------------------------------------
##' Wrapper around mclapply to track progress
##' 
##' Based on http://stackoverflow.com/questions/10984556
##' 
##' @param X         a vector (atomic or list) or an expressions vector. Other
##'                  objects (including classed objects) will be coerced by
##'                  ‘as.list’
##' @param FUN       the function to be applied to
##' @param ...       optional arguments to ‘FUN’
##' @param mc.preschedule see mclapply
##' @param mc.set.seed see mclapply
##' @param mc.silent see mclapply
##' @param mc.cores see mclapply
##' @param mc.cleanup see mclapply
##' @param mc.allow.recursive see mclapply
##' @param mc.progress track progress?
##' @param mc.style    style of progress bar (see txtProgressBar)
##'
##' @examples
##' x <- mclapply2(1:1000, function(i, y) Sys.sleep(0.01))
##' x <- mclapply2(1:3, function(i, y) Sys.sleep(1), mc.cores=1)
##------------------------------------------------------------------------------
mclapply2 <- function(X, FUN, ..., 
                      mc.preschedule = TRUE, mc.set.seed = TRUE,
                      mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
                      mc.cleanup = TRUE, mc.allow.recursive = TRUE,
                      mc.progress=TRUE, mc.style=3) 
{
  if (!is.vector(X) || is.object(X)) X <- as.list(X)
  
  if (mc.progress) {
    f <- fifo(tempfile(), open="w+b", blocking=T)
    p <- parallel:::mcfork()
    pb <- txtProgressBar(0, length(X), style=mc.style)
    setTxtProgressBar(pb, 0) 
    progress <- 0
    if (inherits(p, "masterProcess")) {
      while (progress < length(X)) {
        readBin(f, "double")
        progress <- progress + 1
        setTxtProgressBar(pb, progress) 
      }
      cat("\n")
      parallel:::mcexit()
    }
  }
  tryCatch({
    result <- mclapply(X, function(...) {
      res <- FUN(...)
      if (mc.progress) writeBin(1, f)
      res
    }, 
    mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed,
    mc.silent = mc.silent, mc.cores = mc.cores,
    mc.cleanup = mc.cleanup, mc.allow.recursive = mc.allow.recursive
    )
    
  }, finally = {
    if (mc.progress) close(f)
  })
  result
}

mcmapply2 <- function(FUN, ..., 
                      MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE,
                      mc.preschedule = TRUE, mc.set.seed = TRUE,
                      mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
                      mc.cleanup = TRUE, mc.progress=TRUE, mc.style=3) 
{
  if (mc.progress) {
    f <- fifo(tempfile(), open="w+b", blocking=T)
    p <- parallel:::mcfork()
    pb <- txtProgressBar(0, length(...), style=mc.style)
    setTxtProgressBar(pb, 0) 
    progress <- 0
    if (inherits(p, "masterProcess")) {
      while (progress < length(...)) {
        readBin(f, "double")
        progress <- progress + 1
        setTxtProgressBar(pb, progress) 
      }
      cat("\n")
      parallel:::mcexit()
    }
  }
  tryCatch({
    result <- mcmapply(function(...) {
      res <- FUN(...)
      if (mc.progress) writeBin(1, f)
      res
    }, 
    ...,
    MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES,
    mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed,
    mc.silent = mc.silent, mc.cores = mc.cores,
    mc.cleanup = mc.cleanup
    )
    
  }, finally = {
    if (mc.progress) close(f)
  })
  result
}