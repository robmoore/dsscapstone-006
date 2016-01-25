source("common.R")

if (!file.exists(cleanDbName)) {
  dbCreate(cleanDbName)
}

db <- dbInit(cleanDbName)
dbDelete(db, "tweets")
dbDelete(db, "blogs")
dbDelete(db, "geah")

nLinesRatio <- .3 #.6
nTwitterLines <- 2360148 # can use determine_nlines("final/en_US/en_US.twitter.txt")
nBlogLines <- 899288 # can use determine_nlines("final/en_US/en_US.blogs.txt")
#nBlogLines <- 0
nNewsLines <- 1010242 # can use determine_nlines("final/en_US/en_US.news.txt")
#nNewsLines <- 0
#nTotalLines <- nTwitterLines + nBlogLines + nNewsLines

txts <- hash()
txts$tweets <- sample_lines("final/en_US/en_US.twitter.txt", nTwitterLines * nLinesRatio, nTwitterLines)
txts$blogs <- sample_lines("final/en_US/en_US.blogs.txt", nBlogLines * nLinesRatio, nBlogLines)
txts$news <- sample_lines("final/en_US/en_US.news.txt", nNewsLines * nLinesRatio, nNewsLines)
txts$geah <- scan("green-eggs-and-ham.txt",
             sep = "\n", 
             what = character())

processType <- function(typeName) {
  print(paste("Making sentences for", typeName, "at", date()))
  sentences <- makeSentences(txts[[typeName]])
  
  print(paste("Removing unknown for", typeName, "at", date()))
  removeUnknownFromText(sentences)
}

db$tweets <- processType('tweets')
db$blogs <- processType('blogs')
db$news <- processType('news')
db$geah <- processType('geah')

rm(list =  ls(pattern = "txts"))

# remove stale entries
dbReorganize(db)
