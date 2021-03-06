source("common.R")

db <- dbInit(dbName)

dbTweets <- db$tweets

# if (!exists('memoQueryNgramProbs'))
#   memoQueryNgramProbs <- memoise(queryNgramProbs)

# cl <- makeCluster.default()

db$quiz1 <- read.csv(file = "quiz-1.csv", as.is = TRUE)
db$quiz2 <- read.csv(file = "quiz-2.csv", as.is = TRUE)

# lowercase
# check words against dictionary (that is, change to _UNK_)
# TODO: need to add _S_ to sentences, too based on if we are looking at bigram, trigram, etc. # of varies
# Maybe make parSapply optional or set up at beginning?
#db$quizQuery <- makeSentences(db$quiz$Query)
#db$quiz1Query <- removeUnknownFromText(db$quiz1$Query)
#db$quiz2Query <- removeUnknownFromText(db$quiz2$Query)

#db$quiz1Query <- parSapply(cl, db$quiz1Query, function(x) cleanQuery(x, 2))
#db$quiz2Query <- parSapply(cl, db$quiz2Query, function(x) cleanQuery(x, 2))

# maybe as list?
#r <- parSapply(cl, db$quizQuery, function(ngram) queryNgramProbs(ngram, db$tweetsNgramsPercentages))
#quizResults1 <- parSapply(cl, unname(db$quiz1Query), function(ngram) queryNgramProbs(ngram, tweets))
#quizResults2 <- parSapply(cl, unname(db$quiz2Query), function(ngram) queryNgramProbs(ngram, tweets))

#quizResults1 <- sapply(unname(db$quiz1$Query), function(q) predict.baseline(q))
#quizResults1 <- parSapply(cl, unname(db$quiz1$Query), function(q) predict.baseline(q))
#quizResults2 <- parSapply(cl, unname(db$quiz2$Query), function(q) predict.baseline(q))

#rm(tweets)

#stopCluster(cl)

#on searches:
# lowercase
# check words against dictionary (that is, change to _UNK_)

# on results back, filter out anything ending in _UNK_. that is, it's not a valid word so you'll need to backoff

#predict.baseline <- function(q){
#  q <- removeUnknownFromText(q)
#  q <- cleanQuery(x, 2)
#  r <- queryNgramProbs(ngram, db$tweetsNgramsPercentages)

# combine the hashes in lists
# order by score
# get max 3
# grab last 'word' from ngrams

#pick top three suggestions 
#  c('the', 'on', 'a')
#}

#apply(r, function(x) values(x))

#devtools::install_github("rstudio/profvis")
# library(profvis)
# p <- profvis({
#   #predict.baseline('a case of')
#   sapply(unname(db$quiz2$Query), function(q) predict.baseline(q))
# })

predict.baseline <- function(q) {
  if (!exists("env")) {
    db <- dbInit(dbName)
    env <- new.env()
    dbLazyLoad(db, env, c('tweetsModel'))
  }
  predict.baseline.raw(q, env$tweetsModel)  
}

sapply(db$quiz1$Query, function(q) predict.baseline(q))
