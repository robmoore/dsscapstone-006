set.seed(19394399)

download.maybe <- function(url, refetch=FALSE, path=".") {
  dest <- file.path(path, basename(url))
  if (refetch || !file.exists(dest))
    download.file(url, dest)
  dest
}

download.maybe("http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")

if (!file.exists("final")) {
  unzip("Coursera-SwiftKey.zip")
}
# TODO: Update R packages

# install.packages("quanteda", "LaF", "hashr")
library(LaF) # for sample_lines
library(quanteda)
#library(hashr)
library(parallel)

nLinesRatio <- .6
nTwitterLines <- 2360148 # determine_nlines("final/en_US/en_US.twitter.txt")
nBlogLines <- 899288 # determine_nlines("final/en_US/en_US.blogs.txt")
nNewsLines <- 1010242 # determine_nlines("final/en_US/en_US.news.txt")

tweets <- sample_lines("final/en_US/en_US.twitter.txt", nTwitterLines * nLinesRatio, nTwitterLines)
blogs <- sample_lines("final/en_US/en_US.blogs.txt", nBlogLines * nLinesRatio, nBlogLines)
news <- sample_lines("final/en_US/en_US.news.txt", nNewsLines * nLinesRatio, nNewsLines)

#all <- c(tweets, blogs, news)
all <- tweets
nsentences <- length(all)
all <- tokenize(all, what = "sentence", simplify = TRUE)
#allTokens <- tokenize(toLower(allSentences), removePunc = TRUE, removeNumbers = TRUE)

# replace profanity with UNK tag
# https://gist.githubusercontent.com/ryanlewis/a37739d710ccdb4b406d/raw/0fbd315eb2900bb736609ea894b9bde8217b991a/google_twunter_lol
badWords <- scan("https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en", sep = "\n", what = character())
# TODO: Filter out phrases, just filter on words
badWords <- badWords[grepl("^\\w+$", badWords, perl = TRUE)]
#bwExp <- sapply(badWords, function(x) paste0('(\\s?)', x, '\\w*(\\W)'))
#allMod <- gsub(paste0(bwExp, collapse = "|"), "\\1<UNK>\\2", all, perl = TRUE)
# try again without expanding out word 
# Only replace whole words
#bwExp <- paste0(sapply(badWords, function(x) paste0('\\s?', x, '\\w*\\b')), collapse = "|")
# TODO: Remove URLs /^(https?:\/\/)?([\da-z\.-]+)\.([a-z\.]{2,6})([\/\w \.-]*)*\/?$/
bwExp <- paste0(sapply(badWords, function(x) paste0('\\b', x, '\\b')), collapse = "|")
allMod <- toLower(all)

# Calculate the number of cores
no_cores <- detectCores() - 2
# Initiate cluster
cl <- makeCluster(no_cores, type="FORK", renice = 15)

# TODO: Might make this a sapply call so can parallelize
#allMod <- gsub(paste0(bwExp, collapse = "|"), "_UNK_", allMod, perl = TRUE)
allMod <- parSapply(cl, allMod, function(x) gsub(bwExp, "_UNK_", x, perl = TRUE))

#tokensAll <- tokenize(allMod, removePunct = TRUE, removeNumbers = TRUE)
# grep("_UNK_", allMod)
# TODO: Can probably use theasaurus capabilities of dfm object to sub these values
#mydict <- dictionary(list(UNK=bwExp))
#testDfm <- dfm(phrasetotoken(toLower(all), mydict), thesaurus = lapply(mydict, function(x) gsub("\\s", "_", x)))
#testDfm <- dfm(toLower(mytexts), thesaurus = mydict, valuetype = "regex")

#sum(ntoken(allMod))
#sum(ntype(allMod))

# get unigrams without markers
#allTokens <- tokenize(allMod, removePunc = TRUE, removeNumbers = TRUE)
#unigrams <- ngrams(allTokens, 1)
#sum(ntoken(unigrams))
#sum(ntype(unigrams))
unigramsDfm <- dfm(allMod, removePunct = TRUE, toLower = FALSE, removeNumbers = TRUE, removeTwitter = TRUE)
unigramsDfm <- trim(unigramsDfm, minCount = 1)

# add sentence b/e markers
# TODO: these aren't being separated into sentences so need to revisit
#allMod <- unname(sapply(allMod, function(x) paste("_S_", x, "_S_")))
#tokensAll <- tokenize(allMod, removePunct = TRUE, removeNumbers = TRUE)

# get bigrams and trigrams with markers
#othergrams <- ngrams(tokenize(allMod, removePunct = TRUE), 2:3)
#bigrams <- ngrams(tokensAll, 2)
#sum(ntoken(bigrams))
#sum(ntype(bigrams))
bigramsAllMod <- unname(sapply(allMod, function(x) paste("_S_", x, "_S_")))
bigramsDfm <- dfm(bigramsAllMod, ngrams = 2, removePunct = TRUE, toLower = FALSE, removeNumbers = TRUE, removeTwitter = TRUE)
bigramsDfm <- trim(bigramsDfm, minCount = 2)

# TODO: these aren't being separated into sentences so need to revisit
#allMod <- unname(sapply(allMod, function(x) paste("_S_", x, "_S_")))
#tokensAll <- tokenize(allMod, removePunct = TRUE, removeNumbers = TRUE)

#trigrams <- ngrams(allMod, ngrams = 3, toLower = TRUE, removePunct = TRUE)
#sum(ntoken(trigrams))
#sum(ntype(trigrams))
trigramsAllMod <- unname(sapply(bigramsAllMod, function(x) paste("_S_", x, "_S_")))
trigramsDfm <- dfm(trigramsAllMod, ngrams = 3, removePunct = TRUE, toLower = FALSE, removeNumbers = TRUE, removeTwitter = TRUE)
trigramsDfm <- trim(trigramsDfm, minCount = 3)
#features(trigramsDfm)

quardigramsAllMod <- unname(sapply(trigramsAllMod, function(x) paste("_S_", x, "_S_")))
quadrigramsDfm <- dfm(quardigramsAllMod, ngrams = 4, removePunct = TRUE, toLower = FALSE, removeNumbers = TRUE, removeTwitter = TRUE)
quadrigramsDfm <- trim(quadrigramsDfm, minCount = 4)

quinquegramsAllMod <- unname(sapply(quardigramsAllMod, function(x) paste("_S_", x, "_S_")))
quinquegramsDfm <- dfm(quinquegramsAllMod, ngrams = 5, removePunct = TRUE, toLower = FALSE, removeNumbers = TRUE, removeTwitter = TRUE)
quinquegramsDfm <- trim(quinquegramsDfm, minCount = 5)

#topfeatures(unigramsDfm)["the"]

# create entries for 
#ntokens <- sum(ntoken(allTokens))
#ntypes <- sum(ntype(allTokens))
#features(unigramsDfm)
#topfeatures(unigramsDfm)["the"]
#unigramFeatures <- topfeatures(unigramsDfm, length(features(unigramsDfm)))
# TODO: ignore _S_ in unigramsDfm
#unigramsLogFreq <- weight(unigramsDfm, type = "logFreq")


# provides percentage frequency for
#featureFreq <- function(dfm) {
#  tf <- topfeatures(dfm, nfeature(dfm))
#  # filter out _S_ in case of bi/trigrams
#  #tf <- tf[which(names(tf) != "_S_")]
#  tf / sum(ntoken(dfm))
#}

#unigramsFreq <- featureFreq(unigramsDfm)
#unigramsFreqDfm <- unigramsDfm / sum(ntoken(unigramsDfm))
ngramProbs <- function(ngram, nMinusOneFreq, nFreq) {
  nMinusOneGram <- gsub("^(.*)_.*$", "\\1", ngram, perl = TRUE)
  nMinusOneProb <- ifelse(nMinusOneGram == "_S_", #if gram starting sentence
                      nsentences, # beginning of sentence, so no unigram probability
                      nMinusOneFreq[nMinusOneGram])
  unname(nFreq[ngram] / nMinusOneProb)
}

dfmFreq <- function(ngramDfm) {
  topfeatures(ngramDfm, nfeature(ngramDfm))
}

unigramsFreq <- dfmFreq(unigramsDfm)
bigramsFreq <- dfmFreq(bigramsDfm)
trigramsFreq <- dfmFreq(trigramsDfm)
quadrigramFreq <- dfmFreq(quadrigramsDfm)
quinquegramsFreq <- dfmFreq(quinquegramsDfm)

ngramMLE <- function(ngram, nMinusOneFreq, nFreq) {
  nMinusOneGram <- gsub("^(.*)_.*$", "\\1", ngram, perl = TRUE)
  nMinusOneCount <- ifelse(is.na(nMinusOneFreq[nMinusOneGram]), 
                          nsentences, # likely beginning of sentence, so no unigram probability IS THIS VALID?
                          nMinusOneFreq[nMinusOneGram])
  # could do below by looking up all names for lower order gram and then searching for all upper order with prefix and
  # multiple. but how to store this? and merge it back together?
  unname(nFreq[ngram] / nMinusOneCount) # Might be able to do these all at once if you get all cases? so all "i" for  "i <something>"
}

# TODO: Maybe only calculate these when asked? That is, don't precalculate? Might tie in with backoff, too.
#bigramProbs <- sapply(names(bigramsFreq), function(x) ngramMLE(x, unigramsFreq, bigramsFreq))
bigramProbs <- parSapply(cl, names(bigramsFreq), function(x) ngramMLE(x, unigramsFreq, bigramsFreq))
trigramProbs <- parSapply(cl, names(trigramsFreq), function(x) ngramMLE(x, bigramsFreq, trigramsFreq))
quadrigramProbs <- parSapply(cl, names(quadrigramFreq), function(x) ngramMLE(x, trigramsFreq, quadrigramFreq))
quinquegramProbs <- parSapply(cl, names(quinquegramsFreq), function(x) ngramMLE(x, quadrigramFreq, quinquegramsFreq))

stopCluster(cl)

trigramProbs[grep("^case_of_[^_]*$", names(trigramProbs))]
quadrigramProbs[grep("^a_case_of_[^_]*$", names(quadrigramProbs))]
quinquegramProbs[grep("^and_a_case_of_[^_]*$", names(quinquegramProbs))]

#cbind.dfm: combine dfms
#changeunits: change units "up" or "down" from sentences to words for corpus
# removeFeatures: remove features from objects -> perhaps profanity?

# To figure probability of sentence, have to multiple each bigram prob together

# Perhaps we can just divid dfm objects directly?

# can use unname() to just get the number from a named num list

# TODO: How to save off model to disk?
# saveRDS(mod, "mymodel.rds")
# save(object list,file="myfile.RData")

# TODO: See trim() to shrink dfm size
# Note: Use of nfeature
#freqTerms <- topfeatures(fomcDfm, n = nfeature(fomcDfm))
#freqTerms[freqTerms >= 2]

# normalization of bigrams and trigrams using unigram probabilities?
# isn't this just the conditional probability?
# probability of "i want" / "i"

# take log probabilities to prevent underflow?

# stupid backoff

#http://www.inside-r.org/packages/cran/stringr/docs/str_replace_all

# TODO: Make function for repeated calls
# TODO: Maybe only calculate these when asked? That is, don't precalculate? Might tie in with backoff, too.
#unigramProbs[grep("^of_[^_]*$", names(unigramProbs))]
# just get most frequent word? -> 'the'
bigramProbs <- parSapply(cl, names(bigramsFreq), function(x) ngramMLE(x, unigramsFreq, bigramsFreq))
trigramProbs <- parSapply(cl, names(trigramsFreq), function(x) ngramMLE(x, bigramsFreq, trigramsFreq))
quadrigramProbs <- parSapply(cl, names(quadrigramFreq), function(x) ngramMLE(x, trigramsFreq, quadrigramFreq))
quinquegramProbs <- parSapply(cl, names(quinquegramsFreq), function(x) ngramMLE(x, quadrigramFreq, quinquegramsFreq))

#stopCluster(cl)

# Backoff
# try 5g with 5 words (or match number of words to Ng)
# recurse to 4g, 3g, 2g chopping off end word at each level
# add penalty for each backoff

#cbind.dfm: combine dfms
#changeunits: change units "up" or "down" from sentences to words for corpus
#removeFeatures: remove features from objects -> perhaps profanity?

# TODO: How to save off model to disk?
# saveRDS(mod, "mymodel.rds")
# save(object list,file="myfile.RData")

#freqTerms <- topfeatures(fomcDfm, n = nfeature(fomcDfm))
#freqTerms[freqTerms >= 2]

# can use textfile to load file
#myCorpus <- corpus(x)
#myDfm <- dfm(myCorpus)
#plot(myDfm) # word cloud
#topfeatures(myDfm, 25)

plot(topfeatures(unigramsDfm, 100), log = "y", cex = .6, ylab = "Term frequency")
#unigramsDfmPct <- weight(unigramsDfm, "relFreq") * 100
unigramsDfmPct <- unigramsFreq * 100
#unigramsDfmPct[, "the"] # returns dfm with only "the"
plot(unigramsDfmPct[1:10], type="b", xlab="Top Ten Words", ylab="Percentage of Full Text", xaxt ="n")
axis(1, 1:10, labels = names(unigramsDfmPct[1:10]))

plot(topfeatures(unigramsDfm, 100), log = "y", cex = .6, ylab = "Term frequency")
myDfmPct <- weight(unigramsDfm, "relFreq") * 100
plot(topfeatures(unigramsDfm), type="b",
     +      xlab="Top Ten Words", ylab="Percentage of Full Text", xaxt ="n")
axis(1, 1:10, labels = names(topfeatures(unigramsDfm)))

# In my experience, I found bigrams and trigrams to take up the most memory. Things got
# smaller again when going on to 4-grams and 5-grams. If I recall correctly I kept unigrams 
# with count > 1, bigrams with count > 2, trigrams with count > 3, 4-grams with counts > 4, 
# 5-grams with counts > 5. Next I adjusted these minimum counts upwards until the memory 
# was a reasonable size for a Shiny App. For the quizzes, though, I don't see much need 
# to trim low frequency n-grams.

#library(hashr)
#pGramsHT$gram         <- hash(pGramsHT$gram)

# TODO: Do the same plot with bigrams and trigrams

# When looking up completions, replace profanity with _UNK_!

# parse sentence into bigrams
# for each bigram get the prob
# add the log of each probability together
# when you get to the end of the sentence, try looking up trigrams and?

# try 4grams to predict sentence?