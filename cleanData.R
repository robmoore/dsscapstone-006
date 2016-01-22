source("common.R")
cleanDbName <- "mclapply-test-2.db1"
if (!file.exists(cleanDbName)) {
  dbCreate(cleanDbName)
}

db <- dbInit(cleanDbName)
dbDelete(db, "tweets")
dbDelete(db, "blogs")
dbDelete(db, "geah")

geah <- scan("green-eggs-and-ham.txt",
             sep = "\n", 
             what = character())

nLinesRatio <- .1 #.6
nTwitterLines <- 2360148 # can use determine_nlines("final/en_US/en_US.twitter.txt")
nBlogLines <- 899288 # can use determine_nlines("final/en_US/en_US.blogs.txt")
#nBlogLines <- 0
nNewsLines <- 1010242 # can use determine_nlines("final/en_US/en_US.news.txt")
#nNewsLines <- 0
nTotalLines <- nTwitterLines + nBlogLines + nNewsLines

tweets <- sample_lines("final/en_US/en_US.twitter.txt", nTwitterLines * nLinesRatio, nTwitterLines)
blogs <- sample_lines("final/en_US/en_US.blogs.txt", nBlogLines * nLinesRatio, nBlogLines)
news <- sample_lines("final/en_US/en_US.news.txt", nNewsLines * nLinesRatio, nNewsLines)

# Convert to list of sentences
tweets <- makeSentences(tweets)
blogs <- makeSentences(blogs)
news <- makeSentences(news)
geah <- makeSentences(geah)

db$tweets <- removeUnknownFromText(tweets)
db$blogs <- removeUnknownFromText(blogs)
db$news <- removeUnknownFromText(news)
db$geah <- removeUnknownFromText(geah)

# Stop cluster as we're done with it
#stopCluster(cl)

# remove stale entries
dbReorganize(db)
