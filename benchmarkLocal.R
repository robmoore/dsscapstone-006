source("common.R")

db <- dbInit(dbName)

dbTweets <- db$tweets

cl <- makeCluster.default()

memoQueryNgramProbs <- memoise(queryNgramProbs)

predict.baseline <- function(q){
  print(paste("Predicting for '", q, "'"))
  if (nchar(trimws(q)) == 0)
    q <- "_S___S_"
  else {
    q <- removeUnknownFromSentence(q)
    q <- cleanQuery(q, 2)
    print(paste("Modified query:", q))
  }
  rs <- memoQueryNgramProbs(q, dbTweets)
  
  # combine the hashes in lists
  # order by score
  # get max 3
  # grab last 'word' from ngrams
  
  #pick top three suggestions 
  sapply(values(rs), 
         function(x) as.list(x)) %>>% list.flatten %>>% list.sort(-.) %>>% list.map(ngramTail(.name, 1)) %>>% list.cases(sorted = FALSE) %>>% list.take(3)
}

#apply(r, function(x) values(x))