set.seed(19394399)

# install.packages("quanteda", "LaF", "hashr")
library(LaF) # for sample_lines
library(quanteda)
library(hashr)

nLinesRatio <- 1e-1
nTwitterLines <- 2360148 # determine_nlines("final/en_US/en_US.twitter.txt")
nBlogLines <- 899288 # determine_nlines("final/en_US/en_US.blogs.txt")
nNewsLines <- 1010242 # determine_nlines("final/en_US/en_US.news.txt")

tweets <- sample_lines("final/en_US/en_US.twitter.txt", nTwitterLines * nLinesRatio, nTwitterLines)
blogs <- sample_lines("final/en_US/en_US.blogs.txt", nBlogLines * nLinesRatio, nBlogLines)
news <- sample_lines("final/en_US/en_US.news.txt", nNewsLines * nLinesRatio, nNewsLines)

#all <- c(tweets, blogs, news)
all <- tokenize(c(tweets, blogs, news), what = "sentence", simplify = TRUE)
#allTokens <- tokenize(toLower(allSentences), removePunc = TRUE, removeNumbers = TRUE)

# replace profanity with UNK tag
# https://gist.githubusercontent.com/ryanlewis/a37739d710ccdb4b406d/raw/0fbd315eb2900bb736609ea894b9bde8217b991a/google_twunter_lol
badWords <- scan("https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en", sep = "\n", what = character())
#bwExp <- sapply(badWords, function(x) paste0('(\\s?)', x, '\\w*(\\W)'))
#allMod <- gsub(paste0(bwExp, collapse = "|"), "\\1<UNK>\\2", all, perl = TRUE)
# try again without expanding out word 
bwExp <- sapply(badWords, function(x) paste0('\\s?', x, '\\w*\\b'))
allMod <- toLower(all)
allMod <- gsub(paste0(bwExp, collapse = "|"), " _UNK_ ", all, perl = TRUE)

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
unigramsDfm <- dfm(allMod, removePunct = TRUE)
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
bigramsDfm <- dfm(unname(sapply(allMod, function(x) paste("_S_", x, "_S_"))), ngrams = 2, removePunct = TRUE)
bigramsDfm <- trim(bigramsDfm, minCount = 2)

# TODO: these aren't being separated into sentences so need to revisit
#allMod <- unname(sapply(allMod, function(x) paste("_S_", x, "_S_")))
#tokensAll <- tokenize(allMod, removePunct = TRUE, removeNumbers = TRUE)

#trigrams <- ngrams(allMod, ngrams = 3, toLower = TRUE, removePunct = TRUE)
#sum(ntoken(trigrams))
#sum(ntype(trigrams))
trigramsDfm <- dfm(unname(sapply(allMod, function(x) paste("_S_", x, "_S_"))), ngrams = 3, removePunct = TRUE)
trigramsDfm <- trim(trigramsDfm, minCount = 3)
#features(trigramsDfm)

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
nsentences <- length(all)
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

ngramMLE <- function(ngram, nMinusOneFreq, nFreq) {
  nMinusOneGram <- gsub("^(.*)_.*$", "\\1", ngram, perl = TRUE)
  nMinusOneCount <- ifelse(is.na(nMinusOneFreq[nMinusOneGram]),
                          nsentences, # likely beginning of sentence, so no unigram probability
                          nMinusOneFreq[nMinusOneGram])
  unname(nFreq[ngram] / nMinusOneCount)
}

bigramProbs <- sapply(names(bigramsFreq), function(x) ngramMLE(x, unigramsFreq, bigramsFreq))
trigramProbs <- sapply(names(trigramsFreq), function(x) ngramMLE(x, bigramsFreq, trigramsFreq))

trigramProbs[grep("^case_of_[^_]*$", names(trigramProbs))]

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

## counts for frequency in docs?
#docfreq(dz[,1:20])

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