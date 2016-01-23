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

processType <- function(typeName, removeSingletons = TRUE) {
  dbDelete(modelDb, "typeName")
  print(paste("Calculating percentages for", typeName, "at", date()))
  percentages <- calculatePercentages(cleanData[[typeName]])
  
  print(paste("Creating maps for", typeName, "at", date()))
  ngramMap <- mapGramsWrapper(percentages)
  
  print(paste("Creating model for", typeName, "at", date()))
  makeNgramModel2(percentages, ngramMap)
}

library(inline)
includes <- '#include <sys/wait.h>'
code <- 'int wstat; while (waitpid(-1, &wstat, WNOHANG) > 0) {};'
wait <- cfunction(body=code, includes=includes, convention='.C')

modelDb$tweets <- processType('tweets')
wait()
modelDb$blogs <- processType('blogs')
wait()
modelDb$news <- processType('news')
wait()
modelDb$geah <- processType('geah', FALSE)
wait()

# Perplexity calculation
# The cross-entropy is the average of the negative logarithm of the word probabilities. 
# In Figure 7.2 , next to each probability you can find its negative log2.
#I would like to commend the rapporteur on his work.

print(paste("Completed at", date()))

dbReorganize(modelDb)
