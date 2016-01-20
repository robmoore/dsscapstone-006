library(LaF) # for sample_lines
library(parallel)
library(qdapDictionaries)
library(quanteda)
library(filehash)
library(fastmatch)
library(pipeR)
library(rlist)
library(hash)
#library(memoise)
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

#filehashOption(defaultType = "RDS")
dbName <- "corpus.db1"

download.maybe <- function(url, refetch=FALSE, path=".") {
  dest <- file.path(path, basename(url))
  if (refetch || !file.exists(dest))
    download.file(url, dest)
  dest
}

# Faster lookups on vectors
"%fin%" <- function(x, table) fmatch(x, table, nomatch = 0) > 0

#makeCluster.default <- function() makeCluster(detectCores() * .75, type="FORK") # FORK will not work on Windows
makeCluster.default <- function() makeForkCluster(detectCores() * .75, outfile = "cluster-outfile.txt")

# Takes raw input and breaks out into individual sentences
makeSentences <- function(txt) {
  unlist(parSapply(cl, 
                   txt, 
                   function(x) tokenize(x, what = "sentence", simplify = TRUE)))
}

# Uses dictionary and profanity list to filter out potential token values
removeUnknownFromSentence <-  function(s) {
  if (nchar(s) != 0) {
    sTokens <- tokenize(toLower(s), removePunct = TRUE, removeTwitter = TRUE, removeHyphens = TRUE)
    # UNK if not in dictionary
    sTokens <- unname(sapply(sTokens, function(x) ifelse(x %fin% GradyAugmentedClean, x, "_UNK_")))
    sTokens <- unname(sapply(sTokens, function(x) gsub("http[^ ]*", "_UNK_", x)))
    paste(sTokens, collapse = ' ')
  } else {
    s
  }
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

grabFiles()

#---

# TODO: Do we really need the parSapply or even sapply below? it must not be a list or the tokennize call wouldn't work, right?
createNgramsHash <- function(txt, count, removeSingletons = TRUE) {
  if (count > 1)
    #txt <- parSapply(cl, txt, function(x) paste(paste(rep("_S_", count - 1), collapse = " "), x, paste(rep("_S_", count - 1), collapse = " ")))
    txt <- sapply(txt, function(x) paste(paste(rep("_S_", count - 1), collapse = " "), x, paste(rep("_S_", count - 1), collapse = " ")))

  t <- table(tokenize(txt, ngrams = count, simplify = TRUE))
  # filter out singletons for count > 2
  if (count > 2 && removeSingletons) t <- t[t > 1]
  hash(t)
}

createNgramsHashAll <- function(txt, removeSingletons = TRUE) {
  parSapply(cl, 
            list(unigrams = 1, bigrams = 2, trigrams = 3, quadgrams = 4), 
            function(c) createNgramsHash(txt, c, removeSingletons && c > 2))
#  sapply(list(unigrams = 1, bigrams = 2, trigrams = 3, quadgrams = 4), 
#            function(c) createNgramsHash(txt, c, removeSingletons && c > 2))
}

#createNgramsHashAll <- function(txt, removeSingletons = TRUE) {
#  parSapply(cl, list(unigrams = 1, bigrams = 2, trigrams = 3), 
#            function(c) createNgramsHash(txt, c, removeSingletons))
#}

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
                                                                                                          sentenceCount = sentenceCount))),
       quadgrams = hash(parSapply(cl, keys(ngramsHash$quadgrams), function(key) createOtherGramsPercentages(key, 
                                                                                                            loGramPs = ngramsHash$trigrams, 
                                                                                                            hiGramPs = ngramsHash$quadgrams, 
                                                                                                            sentenceCount = sentenceCount))))
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
ngramToken <- function(ngram, count = 1) {
  stri_match_all_regex(ngram, "(_S_|_UNK_|[^_]+)")[[1]][,1][count]
}

ngramLength <- function(ngram) {
  str(ngram)
  stri_count_regex(ngram, "(_S_|_UNK_|[^_]+)")
}

# adjust ngram down to length of ngram in query + 1 before making this call
matchingKeys <- function(ngram, ngrams) {
  nl <- ngramLength(ngram)
  ks <- keys(ngrams[[nl + 1]])
  ks[grepl(paste0("^", ngram, "_"), # Use fastmatch here?
           ks)]
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
  #str(rs)
  if(length(rs) != 0) 
    length(list.cases(sapply(list.cases(sapply(rs, function(r) keys(r))), function(x) ngramTail(x,1))))
  else 0
}

# TODO: if single ngram (unigram) use rep _S_ at beginning to fill it out?
queryNgramProbs <- function(ngram, ngrams, ks = c(), vs = c(), n = 0, maxResults = 3) {
  #print(paste("ks:", ks))
  #print(paste("count for", ks, ":", countResults(vs)))
  nl <- ngramLength(ngram)
  nsl <- length(ngrams)
  if (nsl <= nl) # If too long for # of ngram lengths we have then trim down
    Recall(ngram = ngramTail(ngram, nsl - 1),
           ngrams = ngrams)
  else if(nl != 0 && countResults(vs) < maxResults)
    Recall(ngram = ngramTail(ngram), 
           ngrams = ngrams, 
           ks = c(ks, ngram),
           vs = c(vs, processValues(ngram, ngrams, n)),
           n = n + 1)
  else 
    hash(ks, vs)
}

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
calculatePercentages <- function(txtV, removeSingletons = TRUE) {
  force(txtV)
  createNgramsPercentagesAll(createNgramsHashAll(txtV, removeSingletons), length(txtV))
}

quotemeta <- function(string) {
  str_replace_all(string, "(\\W)", "\\\\\\1")
}

# setClass("Node",
#          slots = list(ngram = "character", p = "numeric", snodes = "list"))
# makeModel <- function(ngram, ngrams, count = 1, model = hash()) {
#   print(paste("Processing", ngram, ", count:", count))
#   ngramsLength = length(ngrams)
#   p = ngrams[[count]][[ngram]] 
#   sbMultiplier <- ngramsLength - count
#   if (sbMultiplier != 0)
#     p <- p * sbMultiplier * .4
#   node <- new("Node", ngram = ngram, p = p)
#   if (count != ngramsLength) {
#     subnodes <- grep(paste0("^", quotemeta(ngram), "_"), keys(ngrams[[count + 1]]), value = TRUE, perl = TRUE)
#     if (length(subnodes) != 0) {
#       node@snodes <- lapply(subnodes, function(subNgram) makeModel(subNgram, ngrams, count + 1, model))
#     }
#   }
#   model[[ngram]] <- node
#   list(w = ngramTail(ngram, 1), p = p)
# }

# makeModel <- function(ngram, ngrams, count = 1) {
#   print(paste("Processing", ngram, ", count:", count))
#   ngramsLength = length(ngrams)
#   p = ngrams[[count]][[ngram]] 
#   sbMultiplier <- ngramsLength - count
#   if (sbMultiplier != 0)
#     p <- p * sbMultiplier * .4
#   #node <- new("Node", ngram = ngram, p = p)
#   #node <- list(ngram = ngram, w = ngramTail(ngram, 1), p = p)
#   node <- list(ngram = ngram, w = ngramTail(ngram, 1), p = p)
#   r <- list(node)
#   if (count != ngramsLength) {
#     subnodes <- grep(paste0("^", quotemeta(ngram), "_"), keys(ngrams[[count + 1]]), value = TRUE, perl = TRUE)
#     if (length(subnodes) != 0) {
#       snodes <- lapply(subnodes, function(subNgram) makeModel(subNgram, ngrams, count + 1))
#       #print(paste("length of snodes:", length(snodes)))
#       #str(snodes %>>% list.take(1) %>>% list.take(1))
#       #r$snodePs <- snodes %>>% list.take(1) %>>% list.select(w, p) %>>% list.sort((p))
#       r <- c(r, snodes)
#     }
#   }
#   #model[[ngram]] <- node
#   #list(node = node, snodes = snodes)
#   r
# }
# 
# makeNgramModel <- function(ngrams, count = 1) {
#   #parSapply(cl, keys(ngrams[[count]]), function(ngram) makeModel(ngram, ngrams, count))
#   lapply(keys(ngrams[[count]]), function(ngram) makeModel(ngram, ngrams, count))
# }

# TODO: Consider storing the name of the subnode in the node when creating the maps to begin with?
makeModel <- function(ngram, ngrams, count = 1) {
  print(paste("Processing", ngram))
  ngramsLength = length(ngrams)
  p = ngrams[[count]][[ngram]] 
  sbMultiplier <- ngramsLength - count
  if (sbMultiplier != 0)
    p <- p * sbMultiplier * .4
  node <- list(ngram = ngram, p = p)
  if (count != ngramsLength) {
    subnodes <- grep(paste0("^", quotemeta(ngram), "_"), keys(ngrams[[count + 1]]), value = TRUE, perl = TRUE)
    if (length(subnodes) != 0) {
      node$snodeProbs <- lapply(subnodes, 
                                function(subnode) list(w = ngramTail(subnode, 1),
                                                       p = (ngrams[[count + 1]][[subnode]]))) %>>%  list.sort((p))
    }
  }
  node
}

makeNgramModelWrapper <- function(ngrams, count = 1) {
  hash(parSapply(cl, keys(ngrams[[count]]), function(ngram) makeModel(ngram, ngrams, count), simplify = FALSE, USE.NAMES = TRUE))
}

makeNgramModel <- function(ngrams) {
  list(unigrams = makeNgramModelWrapper(ngrams, 1),
       bigrams = makeNgramModelWrapper(ngrams, 2),
       trigrams = makeNgramModelWrapper(ngrams, 3),
       quadgrams = ngrams$quadgrams)
}

# makeNgramModel <- function(ngrams, count = 1) {
#   models <- parSapply(cl, keys(ngrams[[count]]), function(ngram) makeModel(ngram, ngrams, count))
#   
#   #sapply(keys(ngrams[[count]]), function(ngram) makeModel(ngram, ngrams, count))
# }
# 
# makeNgramModel <- function(ngrams, count = 1) {
#   model <- hash()
#   #parSapply(cl, keys(ngrams[[count]]), function(ngram) {
#   #  makeModel(ngram, ngrams, count, model)
#   #})
#   sapply(keys(ngrams[[count]]), function(ngram) makeModel(ngram, ngrams, count, model))
#   model
# }

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
  str(ngram)
  results <- lookupProbs(ngram, ngrams)
  #str(results)
  results %>>% list.sort(-p) %>>% list.group(w) %>>% list.map(list.first(.)) %>>% list.take(3) %>>% list.sort(-p) %>>% unname
}

# findNodes <- function(ngram, h) {
#   if (nchar(ngram) == 0)
#     list()
#   else {
#     node <- h[[ngram]]
#     c(node@snodes, findNodes(ngramTail(ngram), h))
#   }
# }
# 
# findFilteredNodes <- function(ngram, h) {
#   findNodes(ngram, h) %>>% list.sort(-p) %>>% list.group(w) %>>% list.map(list.first(.)) %>>% unname
# }

predict.baseline.raw <- function(q, model) {
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
  
  #rs <- memoQueryNgramProbs(q, dbTweets)
  #rs <- memQueryNgramProbs(q)
  #rs <- findFilteredNodes(q, dbTweetsModel) 
  #env <- new.env()
  #dbLazyLoad(db, env, c('tweetsModel'))
  suggestions <- lookupProbsWrapper(q, model)
  # combine the hashes in lists
  # order by score
  # get max 3
  # grab last 'word' from ngrams
  
  # 1] "ks: __UNK" _S___S___UNK_"
  # ks: UNK__record" _UNK___UNK__record"
  
  #print(rs)
  #c('the','be','to')
  #pick top three suggestions 
  #suggestions <- sapply(values(rs), 
  #                      function(x) as.list(x)) %>>% list.flatten %>>% list.sort(-.) %>>% list.map(ngramTail(.name, 1)) %>>% list.cases(sorted = FALSE) %>>% list.take(3)
  #str(rs)
#   suggestions <- sapply(rs, 
#                         function(x) list.map(w) %>>% list.take(3))
  print(paste("Suggestions:", paste(suggestions, collapse = ",")))
  #print(paste("Suggestions:", paste(rs, collapse = ",")))
  #print(suggestions)
  #str(suggestions)
  suggestions %>>% list.mapv(w) #%>>% list.flatten(use.names = FALSE)
}
