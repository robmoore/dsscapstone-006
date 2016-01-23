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

print("Processing tweets")
modelTweets <- calculatePercentages(cleanData$tweets)
print("Processing blogs")
modelBlogs <- calculatePercentages(cleanData$blogs)
print("Processing news")
modelNews <- calculatePercentages(cleanData$news)
print("Processing geah")
modelGeah <- calculatePercentages(cleanData$geah, removeSingletons = FALSE)

print(paste("Creating maps at", date()))
print("Processing tweets")
mapTweets <- mapGramsWrapper(modelTweets)
print("Processing blogs")
mapBlogs <- mapGramsWrapper(modelBlogs)
print("Processing news")
mapNews <- mapGramsWrapper(modelNews)
print("Processing geah")
mapGeah <- mapGramsWrapper(modelGeah)

print(paste("Creating model at", date()))
# modelDb$tweets <- makeNgramModel(modelTweets)
# modelDb$blogs <- makeNgramModel(modelBlogs)
# modelDb$news <- makeNgramModel(modelNews)
# modelDb$geah <- makeNgramModel(modelGeah)

print("Processing tweets")
modelDb$tweets <- makeNgramModel2(modelTweets, mapTweets)
print("Processing blogs")
modelDb$blogs <- makeNgramModel2(modelBlogs, mapBlogs)
print("Processing news")
modelDb$news <- makeNgramModel2(modelNews, mapNews)
print("Processing geah")
modelDb$geah <- makeNgramModel2(modelGeah, mapGeah)

rm(list = ls(pattern = c("*Tweets", "*Blogs", "*News", "*Geah")))

# Perplexity calculation
# The cross-entropy is the average of the negative logarithm of the word probabilities. 
# In Figure 7.2 , next to each probability you can find its negative log2.
#I would like to commend the rapporteur on his work.

print(paste("Completed at", date()))

dbReorganize(modelDb)
