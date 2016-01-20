source("common.R")

if (!file.exists(dbName)) {
 # file.remove("corpus/*")
  #file.copy(dbName, paste0("corpus-", format(Sys.time(), "%Y.%M.%d"), ".db1"))
  #dbUnlink(dbName)
  #file.remove("corpus.db1")
  dbCreate(dbName)
}

db <- dbInit(dbName)
dbDelete(db, "tweets")
dbDelete(db, "blogs")
dbDelete(db, "geah")

db$geah <- scan("green-eggs-and-ham.txt",
                        sep = "\n", 
                        what = character())

nLinesRatio <- .1 #.6
nTwitterLines <- 2360148 # can use determine_nlines("final/en_US/en_US.twitter.txt")
nBlogLines <- 899288 # can use determine_nlines("final/en_US/en_US.blogs.txt")
#nBlogLines <- 0
#nNewsLines <- 1010242 # can use determine_nlines("final/en_US/en_US.news.txt")
nNewsLines <- 0
nTotalLines <- nTwitterLines + nBlogLines + nNewsLines

db$tweets <- sample_lines("final/en_US/en_US.twitter.txt", nTwitterLines * nLinesRatio, nTwitterLines)
db$blogs <- sample_lines("final/en_US/en_US.blogs.txt", nBlogLines * nLinesRatio, nBlogLines)
#db$news <- sample_lines("final/en_US/en_US.news.txt", nNewsLines * nLinesRatio, nNewsLines)

# Initiate cluster for parallel operations
cl <- makeCluster.default()

# Convert to list of sentences
db$tweets <- makeSentences(db$tweets)
db$blogs <- makeSentences(db$blogs)
#db$news <- makeSentences(db$news)
db$geah <- makeSentences(db$geah)

db$tweets <- removeUnknownFromText(db$tweets)
db$blogs <- removeUnknownFromText(db$blogs)
#db$news <- removeUnknownFromText(db$news)
db$geah <- removeUnknownFromText(db$geah)

# Stop cluster as we're done with it
stopCluster(cl)

#rm(GradyAugmentedClean)
# remove stale entries
#dbReorganize(db)
#db <- dbInit(dbName)
