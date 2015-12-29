# 1. Use closed vocabulary/dictionary to limit number of features
# 2. calculate probabilities on demand rather than prior
# 3. calculate MLE in a group at once (use n-1 gram to determine ngrams to process)
# 4. remove URLs, other?
# 5. hash feature entries

set.seed(19394399)

download.maybe <- function(url, refetch=FALSE, path=".") {
  dest <- file.path(path, basename(url))
  if (refetch || !file.exists(dest))
    download.file(url, dest)
  dest
}
download.maybe("http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")

# unzip file if not previously done
if (!file.exists("final")) {
  unzip("Coursera-SwiftKey.zip")
}

library(LaF) # for sample_lines
library(quanteda)
library(parallel)

nLinesRatio <- .6
nTwitterLines <- 2360148 # can use determine_nlines("final/en_US/en_US.twitter.txt")
nBlogLines <- 899288 # can use determine_nlines("final/en_US/en_US.blogs.txt")
nNewsLines <- 1010242 # can use determine_nlines("final/en_US/en_US.news.txt")

tweets <- sample_lines("final/en_US/en_US.twitter.txt", nTwitterLines * nLinesRatio, nTwitterLines)
blogs <- sample_lines("final/en_US/en_US.blogs.txt", nBlogLines * nLinesRatio, nBlogLines)
news <- sample_lines("final/en_US/en_US.news.txt", nNewsLines * nLinesRatio, nNewsLines)

all <- c(tweets, blogs, news)
all <- tokenize(all, what = "sentence", simplify = TRUE)
nsentences <- length(all)

# replace profanity with UNK tag
excluded <- scan("https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en", 
                 sep = "\n", 
                 what = character())
# Filter out phrases, just use words
excluded <- excluded[grepl("^\\w+$", excluded, perl = TRUE)]
# Remove URLs
#excluded <- c(excluded, "(https?:\\/\\/)?([\\da-z\\.-]+)\\.([a-z\\.]{2,6})([\\/\\w \\.-]*)*\\/?)")
# Only replace whole words
excluded <- paste0(sapply(excluded, function(x) paste0('\\b', x, '\\b')), collapse = "|")
# Convert to lower case to ease comparisons
allMod <- toLower(all)

# Initiate cluster for parallel operations
cl <- makeCluster(detectCores() - 2, type="FORK", renice = 15)
# Remove words to be excluded using _UNK_ placeholder
allMod <- parSapply(cl, allMod, function(x) gsub(excluded, "_UNK_", x, perl = TRUE))
# Stop cluster as we're done with it
stopCluster(cl)

# Create document-feature matrices (DFMs) for each ngram type (2-5)
unigramsDfm <- dfm(allMod, removePunct = TRUE, toLower = FALSE, removeNumbers = TRUE, removeTwitter = TRUE)
unigramsDfm <- trim(unigramsDfm, minCount = 2)

# add sentence begin/end markers
bigramsAllMod <- unname(sapply(allMod, function(x) paste("_S_", x, "_S_")))
bigramsDfm <- dfm(bigramsAllMod, ngrams = 2, removePunct = TRUE, toLower = FALSE, removeNumbers = TRUE, removeTwitter = TRUE)
bigramsDfm <- trim(bigramsDfm, minCount = 2)

trigramsAllMod <- unname(sapply(bigramsAllMod, function(x) paste("_S_", x, "_S_")))
trigramsDfm <- dfm(trigramsAllMod, ngrams = 3, removePunct = TRUE, toLower = FALSE, removeNumbers = TRUE, removeTwitter = TRUE)
trigramsDfm <- trim(trigramsDfm, minCount = 2)

quardigramsAllMod <- unname(sapply(trigramsAllMod, function(x) paste("_S_", x, "_S_")))
quadrigramsDfm <- dfm(quardigramsAllMod, ngrams = 4, removePunct = TRUE, toLower = FALSE, removeNumbers = TRUE, removeTwitter = TRUE)
quadrigramsDfm <- trim(quadrigramsDfm, minCount = 2)

quinquegramsAllMod <- unname(sapply(quardigramsAllMod, function(x) paste("_S_", x, "_S_")))
quinquegramsDfm <- dfm(quinquegramsAllMod, ngrams = 5, removePunct = TRUE, toLower = FALSE, removeNumbers = TRUE, removeTwitter = TRUE)
quinquegramsDfm <- trim(quinquegramsDfm, minCount = 2)

# Extract features with counts from each DFM
dfmFreq <- function(ngramDfm) {
  topfeatures(ngramDfm, nfeature(ngramDfm))
}

unigramsFreq <- dfmFreq(unigramsDfm)
bigramsFreq <- dfmFreq(bigramsDfm)
trigramsFreq <- dfmFreq(trigramsDfm)
quadrigramFreq <- dfmFreq(quadrigramsDfm)
quinquegramsFreq <- dfmFreq(quinquegramsDfm)

plot(topfeatures(unigramsDfm, 100), log = "y", cex = .6, ylab = "Term frequency")
unigramsDfmPct <- unigramsFreq * 100
plot(unigramsDfmPct[1:10], type="b", xlab="Top Ten Words", ylab="Percentage of Full Text", xaxt ="n")
axis(1, 1:10, labels = names(unigramsDfmPct[1:10]))

# ORIGINAL from above plot
#plot(topfeatures(unigramsDfm, 100), log = "y", cex = .6, ylab = "Term frequency")
#myDfmPct <- weight(unigramsDfm, "relFreq") * 100
#plot(topfeatures(unigramsDfm), type="b",
#     +      xlab="Top Ten Words", ylab="Percentage of Full Text", xaxt ="n")
#axis(1, 1:10, labels = names(topfeatures(unigramsDfm)))

#word clouds
#immigDfm <- dfm(subset(immigCorpus, party=="BNP"), ignoredFeatures = stopwords("english"))
#plot(immigDfm, random.color = TRUE, rot.per = .25, colors = sample(colors()[2:128], 5))
#
#plot(mydfm, max.words=100, colors = brewer.pal(6, "Dark2"), scale=c(8, .5))

#mydfm <- dfm(subset(inaugCorpus, Year>1980))
#mydfmSW <- dfm(subset(inaugCorpus, Year>1980), ignoredFeatures=stopwords("english"))
#results <- data.frame(TTR = lexdiv(mydfm, "TTR"),
#                      CTTR = lexdiv(mydfm, "CTTR"),
#                      U = lexdiv(mydfm, "U"),
#                      TTRs = lexdiv(mydfmSW, "TTR"),
#                      CTTRs = lexdiv(mydfmSW, "CTTR"),
#                      Us = lexdiv(mydfmSW, "U"))
#results
#cor(results)
#t(lexdiv(mydfmSW, "Maas"))
