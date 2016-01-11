source("common.R")

db <- dbInit(dbName)

cl <- makeCluster.default()
setDefaultCluster(cl)

predict.baseline <- function(q){
  q <- removeUnknownFromText(q)
  q <- cleanQuery(x, 2)
  r <- queryNgramProbs(ngram, db$tweetsNgramsPercentages)

  # combine the hashes in lists
  # order by score
  # get max 3
  # grab last 'word' from ngrams

  #pick top three suggestions 
  c('the', 'on', 'a')
}

#apply(r, function(x) values(x))