#install.packages("quanteda", "LaF", "parallel", "textcat")

# For reproducibility's sake
set.seed(19394399)

download.maybe <- function(url, refetch=FALSE, path=".") {
  dest <- file.path(path, basename(url))
  if (refetch || !file.exists(dest))
    download.file(url, dest)
  dest
}
download.maybe("http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")

# unzip file if not previously done
if (!file.exists("final")) { # final dir contains files
  unzip("Coursera-SwiftKey.zip")
}

# Download profanity list
download.maybe("https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en")
# Words to be excluded from text, eg profanity
excluded <- scan("en", 
                 sep = "\n", 
                 what = character())
 # Filter out phrases, just use words
 excluded <- excluded[grepl("^\\w+$", excluded, perl = TRUE)]
 # Only replace whole words
excluded <- paste0(sapply(excluded, function(x) paste0('\\b', x, '\\b')), collapse = "|")

library(LaF) # for sample_lines
library(quanteda)
library(parallel)
library(RColorBrewer)

nLinesRatio <- .6
nTwitterLines <- 2360148 # can use determine_nlines("final/en_US/en_US.twitter.txt")
nBlogLines <- 899288 # can use determine_nlines("final/en_US/en_US.blogs.txt")
nNewsLines <- 1010242 # can use determine_nlines("final/en_US/en_US.news.txt")
nTotalLines <- nTwitterLines + nBlogLines + nNewsLines

tweets <- sample_lines("final/en_US/en_US.twitter.txt", nTwitterLines * nLinesRatio, nTwitterLines)
blogs <- sample_lines("final/en_US/en_US.blogs.txt", nBlogLines * nLinesRatio, nBlogLines)
news <- sample_lines("final/en_US/en_US.news.txt", nNewsLines * nLinesRatio, nNewsLines)

tweets <- tokenize(tweets, what = "sentence", simplify = TRUE)
blogs <- tokenize(blogs, what = "sentence", simplify = TRUE)
news <- tokenize(news, what = "sentence", simplify = TRUE)

# Remove words to be excluded using _UNK_ placeholder
removeExcluded <- function(txt) {
  # replace profanity with UNK tag
  # Convert to lower case to ease comparisons
  txt <- toLower(txt)
  
  ptm <- proc.time()
  txt <- parSapply(cl, txt, function(x) gsub(excluded, "_UNK_", x, perl = TRUE))
  proc.time() - ptm
  
  txt
}

# Initiate cluster for parallel operations
cl <- makeCluster(detectCores() / 2, type="FORK") # FORK will not work on Windows

tweets <- removeExcluded(tweets)
blogs <- removeExcluded(blogs)
news <- removeExcluded(news)

# Stop cluster as we're done with it
stopCluster(cl)

makeDfm <- function(txt, ngramCount) {
  if (ngramCount > 1) {
    txt <- unname(sapply(txt, function(x) paste(paste(rep("_S_", ngramCount - 1), collapse = " "), 
                                                x, 
                                                paste(rep("_S_", ngramCount - 1), collapse = " "))))
  }
  txtDfm <- dfm(txt, 
                removePunct = TRUE, 
                toLower = FALSE, 
                removeNumbers = TRUE, 
                removeTwitter = TRUE, 
                ngrams = ngramCount)
  #trim(txtDfm, minCount = 2) # Remove features with less than 2 counts
  txtDfm
}

# Make list of ngrams 1-3 for each corpus
tweetsUnigram <- makeDfm(tweets, 1)
tweetsBigram <- makeDfm(tweets, 2)
tweetsTrigram <- makeDfm(tweets, 3)

blogsUnigram <- makeDfm(blogs, 1)
blogsBigram <- makeDfm(blogs, 2)
blogsTrigram <- makeDfm(blogs, 3)

newsUnigram <- makeDfm(news, 1)
newsBigram <- makeDfm(news, 2)
newsTrigram <- makeDfm(news, 3)

# Extract features with counts from each DFM
makeDfmFreq <- function(ngramDfm) {
  topfeatures(ngramDfm, nfeature(ngramDfm))
}

# Make list of ngrams freq for each corpus
tweetsUnigramFreq <- makeDfmFreq(tweetsUnigram)
tweetsBigramFreq <- makeDfmFreq(tweetsBigram)
tweetsTrigramFreq <- makeDfmFreq(tweetsTrigram)

blogsUnigramFreq <- makeDfmFreq(blogsUnigram)
blogsBigramFreq <- makeDfmFreq(blogsBigram)
blogsTrigramFreq <- makeDfmFreq(blogsTrigram)

newsUnigramFreq <- makeDfmFreq(newsUnigram)
newsBigramFreq <- makeDfmFreq(newsBigram)
newsTrigramFreq <- makeDfmFreq(newsTrigram)

# Basic counts
tweetsSentences <- length(tweetsUnigram)
tweetsTokens <- sum(ntoken(tweetsUnigram))
tweetsTypes <- sum(ntype(tweetsUnigram))

blogsSentences <- length(blogsUnigram)
blogsTokens <- sum(ntoken(blogsUnigram))
blogsTypes <- sum(ntype(blogsUnigram))

newsSentences <- length(newsUnigram)
newsTokens <- sum(ntoken(newsUnigram))
newsTypes <- sum(ntype(newsUnigram))

totalSentences <- tweetsSentences + blogsSentences + newsSentences
totalTokens <- tweetsTokens + blogsTokens + newsTokens
totalTypes <- tweetsTypes + blogsTypes + newsTypes

old.par <- par(mfrow=c(1, 3))

plot(tweetsUnigram, max.words=100, colors = brewer.pal(6, "Dark2"), scale=c(8, .5))
tweetsUnigramWC <- recordPlot()
plot.new() ## clean up device
tweetsUnigramWC # redraw

plot(blogsUnigram, max.words=100, colors = brewer.pal(6, "Dark2"), scale=c(8, .5))
blogsUnigramWC <- recordPlot()
plot.new() ## clean up device
blogsUnigramWC # redraw

plot(newsUnigram, max.words=100, colors = brewer.pal(6, "Dark2"), scale=c(8, .5))
newsUnigramWC <- recordPlot()
plot.new() ## clean up device
newsUnigramWC # redraw

par(old.par)

save(list = ls(pattern = paste0(c("*Freq", 
                                  "*Sentences", 
                                  "*Tokens", 
                                  "*Types", 
                                  "n*Lines",
                                  "*UnigramWC"), collapse="|")) , 
     file= "all.RData")
