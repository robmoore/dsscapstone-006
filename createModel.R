source("common.R")

cleanDb <- dbInit(cleanDbName)
cleanData <- as(cleanDb, "list")

processType <- function(typeName, removeSingletons = TRUE) {
  modelDbName <- paste0(typeName, "-model.db1")
  if (!file.exists(modelDbName)) {
    dbCreate(modelDbName)
  }
  modelDb <- dbInit(modelDbName)
  
  print(paste("Calculating percentages for", typeName, "at", date()))
  percentages <- calculatePercentages(cleanData[[typeName]])
  
  print(paste("Creating maps for", typeName, "at", date()))
  ngramMap <- mapGramsWrapper(percentages)
  
  print(paste("Creating model for", typeName, "at", date()))
  modelDb$model <- makeNgramModel2(percentages, ngramMap)
  
  dbReorganize(modelDb)
}

processType('tweets')
processType('blogs')
processType('news')
processType('geah', FALSE)

# Perplexity calculation
# The cross-entropy is the average of the negative logarithm of the word probabilities. 
# In Figure 7.2 , next to each probability you can find its negative log2.
#I would like to commend the rapporteur on his work.

print(paste("Completed at", date()))
