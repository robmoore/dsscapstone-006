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
download.maybe("https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en")

# unzip file if not previously done
if (!file.exists("final")) {
  unzip("Coursera-SwiftKey.zip")
}

library(LaF) # for sample_lines
library(quanteda)
library(parallel)
library(textcat)

nLinesRatio <- .01
nTwitterLines <- 2360148 # can use determine_nlines("final/en_US/en_US.twitter.txt")
nBlogLines <- 899288 # can use determine_nlines("final/en_US/en_US.blogs.txt")
nNewsLines <- 1010242 # can use determine_nlines("final/en_US/en_US.news.txt")

tweets <- sample_lines("final/en_US/en_US.twitter.txt", nTwitterLines * nLinesRatio, nTwitterLines)
blogs <- sample_lines("final/en_US/en_US.blogs.txt", nBlogLines * nLinesRatio, nBlogLines)
news <- sample_lines("final/en_US/en_US.news.txt", nNewsLines * nLinesRatio, nNewsLines)

#all <- c(tweets, blogs, news) # treat as single corpus
#all <- tokenize(all, what = "sentence", simplify = TRUE)

# Initiate cluster for parallel operations
cl <- makeCluster(detectCores() / 2, type="FORK")

# Maybe lapply to keep these distinct?
corpus <- parSapply(cl, 
                    c(tweets, blogs, news),
                    function(x) tokenize(x, what = "sentence", simplify = TRUE))

# Remove words to be excluded using _UNK_ placeholder
removeProfanity <- function(txt) {
  # replace profanity with UNK tag
  excluded <- scan("en", 
                   sep = "\n", 
                   what = character())
  # Filter out phrases, just use words
  excluded <- excluded[grepl("^\\w+$", excluded, perl = TRUE)]
  # Only replace whole words
  excluded <- paste0(sapply(excluded, function(x) paste0('\\b', x, '\\b')), collapse = "|")
  # Convert to lower case to ease comparisons
  txt <- toLower(txt1)
  
  ptm <- proc.time()
  txt <- parSapply(cl, txt, function(x) gsub(excluded, "_UNK_", x, perl = TRUE))
  proc.time() - ptm
  
  txt
}

corpus <- parSapply(cl, 
                    corpus,
                    removeProfanity)

makeDfm < function(txt, ngramCount) {
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
  #trim(txtDfm, minCount = 2)
  txtDfm
}

# Make list of ngrams 1-3 for each corpus
corpus <- sapply(corpus, parSapply(cl, txt, makeDfm, 1:3))

# Extract features with counts from each DFM
makeDfmFreq <- function(ngramDfm) {
  topfeatures(ngramDfm, nfeature(ngramDfm))
}

# Make list of frequencies for tweets
tweetNGramsFreq <- parSapply(cl, tweetsNgrams, makeDfmFreq)

# Stop cluster as we're done with it
stopCluster(cl)

