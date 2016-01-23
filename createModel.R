source("common.R")

#cleanDbName <- "mclapply-test.db1"
cleanDb <- dbInit(cleanDbName)

#modelDbName <- "mclapply-test-2.db1"
if (!file.exists(modelDbName)) {
  dbCreate(modelDbName)
}

modelDb <- dbInit(modelDbName)
dbDelete(modelDb, "tweets")
dbDelete(modelDb, "blogs")
dbDelete(modelDb, "news")
dbDelete(modelDb, "geah")

cleanData <- as(cleanDb, "list")

print(paste("Calculating percentages at", date()))

modelTweets <- calculatePercentages(cleanData$tweets)
modelBlogs <- calculatePercentages(cleanData$blogs)
modelNews <- calculatePercentages(cleanData$news)
modelGeah <- calculatePercentages(cleanData$geah, removeSingletons = FALSE)

print(paste("Creating model at", date()))
modelDb$tweets <- makeNgramModel(modelTweets)
modelDb$blogs <- makeNgramModel(modelBlogs)
modelDb$news <- makeNgramModel(modelNews)
modelDb$geah <- makeNgramModel(modelGeah)

rm(list = ls(pattern = c("*Tweets", "*Blogs", "*News", "*Geah")))

# Perplexity calculation
# The cross-entropy is the average of the negative logarithm of the word probabilities. 
# In Figure 7.2 , next to each probability you can find its negative log2.
#I would like to commend the rapporteur on his work.

print(paste("Completed at", date()))

dbReorganize(modelDb)
