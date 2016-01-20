source("common.R")

db <- dbInit(dbName)

#lazyDb <- new.env()
#dbLazyLoad(db, lazyDb, c('tweets', 'blogs', 'geah'))

cl <- makeCluster.default()

print("Calculating percentages")
tweets <- calculatePercentages(db$tweets)
blogs <- calculatePercentages(db$blogs)
#db$news <- calculatePercentages(db$news)
geah <- calculatePercentages(db$geah, removeSingletons = FALSE)

print("Creating model")
db$tweetsModel <- makeNgramModel(tweets)
db$blogsModel <- makeNgramModel(blogs)
#db$newsModel <- makeNgramModel(db$news)
db$geahModel <- makeNgramModel(geah)

# Perplexity calculation
# The cross-entropy is the average of the negative logarithm of the word probabilities. 
# In Figure 7.2 , next to each probability you can find its negative log2.
#I would like to commend the rapporteur on his work.

stopCluster(cl)

dbReorganize(db)
