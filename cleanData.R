source("common.R")

# For reproducibility's sake
set.seed(19394399)

grabFiles()

# Words to be excluded from text, eg profanity
excluded <- scan("profanity.csv", 
                 sep = "\n", 
                 what = character())

# Remove profanity from our dictionary
GradyAugmentedClean <- setdiff(GradyAugmented, excluded)
rm(excluded)

if (file.exists("corpus")) {
  file.remove("corpus/*")
  file.remove("corpus")
}

dbCreate(dbName)
db <- dbInit(dbName)

db$geah <- scan("green-eggs-and-ham.txt",
                        sep = "\n", 
                        what = character())

nLinesRatio <- .1 #.6
nTwitterLines <- 2360148 # can use determine_nlines("final/en_US/en_US.twitter.txt")
nBlogLines <- 899288 # can use determine_nlines("final/en_US/en_US.blogs.txt")
nNewsLines <- 1010242 # can use determine_nlines("final/en_US/en_US.news.txt")
nTotalLines <- nTwitterLines + nBlogLines + nNewsLines

db$tweets <- sample_lines("final/en_US/en_US.twitter.txt", nTwitterLines * nLinesRatio, nTwitterLines)
db$blogs <- sample_lines("final/en_US/en_US.blogs.txt", nBlogLines * nLinesRatio, nBlogLines)
db$news <- sample_lines("final/en_US/en_US.news.txt", nNewsLines * nLinesRatio, nNewsLines)

# Initiate cluster for parallel operations
cl <- makeCluster.default()

# Convert to list of sentences
db$tweets <- makeSentences(db$tweets)
db$blogs <- makeSentences(db$blogs)
db$news <- makeSentences(db$news)
db$geah <- makeSentences(db$geah)

db$tweets <- removeUnknownFromText(db$tweets)
db$blogs <- removeUnknownFromText(db$blogs)
db$news <- removeUnknownFromText(db$news)
db$geah <- removeUnknownFromText(db$geah)

# Stop cluster as we're done with it
stopCluster(cl)

#rm(GradyAugmentedClean)
# remove stale entries
#dbReorganize(db)
#db <- dbInit(dbName)
