source("common.R")

db <- dbInit(dbName)

tweetsSentenceCount <- length(db$tweets)
blogsSentenceCount <- length(db$blogs)
newsSentenceCount <- length(db$news)
geahSentenceCount <- length(db$geah)

# Make ngrams
cl <- makeCluster.default()

db$tweets <- createNgramsHashAll(db$tweets)
db$blogs <- createNgramsHashAll(db$blogs)
db$news <- createNgramsHashAll(db$news)
db$geah <- createNgramsHashAll(db$geah, removeSingletons = FALSE)

# Create percentages (frequency-based)
db$tweets <- createNgramsPercentagesAll(db$tweets, tweetsSentenceCount)
db$blogs <- createNgramsPercentagesAll(db$blogs, blogsSentenceCount)
db$news <- createNgramsPercentagesAll(db$news, newsSentenceCount)
db$geah <- createNgramsPercentagesAll(db$geah, geahSentenceCount)

stopCluster(cl)

# Stupid backoff lookup

# Determine length of text to know where to start or maybe just stupidly search

# sam_i_am
# have to use ngram + 1 (so trigram in sam_i case)
# and look for i_am so whatever my largest ngram is I need to have a key that is one word less
# trigrams[i_am] -> need to find keys for all trigrams that start with this and look up their props
# and then back off to next level and do same thing

# this_is = lookup in trigrams for next word
# this = lookup in bigrams for next word

