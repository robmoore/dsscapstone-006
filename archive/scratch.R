
# Tasks to accomplish

1. Tokenization - identifying appropriate tokens such as words, punctuation, and numbers. Writing a function 
that takes a file as input and returns a tokenized version of it.

2. Profanity filtering - removing profanity and other words you do not want to predict.

# Tips, tricks, and hints

Loading the data in. This dataset is fairly large. We emphasize that you don't necessarily need to load the 
entire dataset in to build your algorithms (see point 2 below). At least initially, you might want to use a 
smaller subset of the data. Reading in chunks or lines using R's readLines or scan functions can be useful. 
You can also loop over each line of text by embedding readLines within a for/while loop, but this may be 
slower than reading in large chunks at a time. Reading pieces of the file at a time will require the use 
of a file connection in R. For example, the following code could be used to read the first few lines of 
the English Twitter dataset:
  
con <- file("en_US.twitter.txt", "r") readLines(con, 1) ## Read the first line of text 
readLines(con, 1) ## Read the next line of text 
readLines(con, 5) ## Read in the next 5 lines of text 
close(con) ## It's important to close the connection when you are done

See the ?connections help page for more information.

Sampling. To reiterate, to build models you don't need to load in and use all of the data. Often relatively few randomly selected rows or chunks need to be included to get an accurate approximation to results that would be obtained using all the data. Remember your inference class and how a representative sample can be used to infer facts about a population. You might want to create a separate sub-sample dataset by reading in a random subset of the original data and writing it out to a separate file. That way, you can store the sample and not have to recreate it every time. You can use the rbinom function to "flip a biased coin" to determine whether you sample a line of text or not.

If you need a refresher on regular expressions, take a look at Jeff Leek's lectures from Getting and Cleaning Data: Part 1 Part 2
-
  
# Had to do this to get R to find Java:
# sudo apt-get install r-cran-rjava
# sudo R CMD javareconf
    
# Take a look at PCorpus (persistent corpus in TM) and 
# filehash package in R -- https://cran.r-project.org/web/packages/filehash/vignettes/filehash.pdf
  
install.packages(c("NLP", "openNLP", "RWeka", "qdap", "LaF", "textcat"))

library(rJava)
library(NLP)
library(openNLP)
library(RWeka)
library(LaF)
library(qdap)
library(textcat)

nlines <- 2360148 # determine_nlines("final/en_US/en_US.twitter.txt")
set.seed(nlines)
x <- sample_lines("final/en_US/en_US.twitter.txt", nlines * 1e-3, nlines) # LaF
xStr <- as.String(x) # NLP

word.ann <- Maxent_Word_Token_Annotator()
sent.ann <- Maxent_Sent_Token_Annotator()

anns <- annotate(xStr, list(sent.ann, word.ann))

# TODO: break tweets into sentences and then use each sentence indendently -- that is, no cross-sentence linking

bad <- scan("https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en",what = character())
rm_stopwords(x, Top200Words, strip = TRUE) # qdap
rm_stopwords(x, c(Top200Words, bad), strip = TRUE) # maybe combine Top200Words + bad?
# TODO: remove utf-8 chars?
# try rm_stopwords with combo of top n words + bad-wrods txt
# http://www.cs.cmu.edu/~biglou/resources/bad-words.txt
ngrams(x) # qdap and NLP

# remove words with non-ASCII cbad2
haracters
# assuming you read your txt file in as a vector, eg. 
# dat <- readLines('~/temp/dat.txt')
dat <- "Special,  satisfação, Happy, Sad, Potential, für"
# convert string to vector of words
dat2 <- unlist(strsplit(dat, split=", "))
# find indices of words with non-ASCII characters
dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
# subset original vector of words to exclude words with non-ASCII char
dat4 <- dat2[-dat3]
# convert vector back to a string
dat5 <- paste(dat4, collapse = ", ")

# Keep hashtags
strip("hello #hastag @money yeah!! o.k.", char.keep="#")

# example from NLP docs
s <- "The quick brown fox jumps over the lazy dog"
## Split into words:
w <- strsplit(s, " ", fixed = TRUE)[[1L]]
## Word tri-grams:
ngrams(w, 3L)
## Word tri-grams pasted together:
vapply(ngrams(w, 3L), paste, "", collapse = " ")

# language detection
textcat(c(
  "This is an English sentence.",
  "Das ist ein deutscher Satz.",
  "Esta es una frase en espa~nol."))

library(quanteda)
# can use textfile to load file
myCorpus <- corpus(x)
myDfm <- dfm(myCorpus)
plot(myDfm) # word cloud
topfeatures(myDfm, 25)

# keep or drop hashtags?
tokensAll <- tokenize(toLower(x), removePunct = TRUE)
tokensNgrams <- ngrams(tokensAll, 2)
features(dfm(tokensNgrams))
# could use the features output above to create frequency tables?
plot(topfeatures(myDfm, 100), log = "y", cex = .6, ylab = "Term frequency")
myDfmPct <- weight(myDfm, "relFreq") * 100
myDfmPct[, "the"]
Document-feature matrix of: 2,360 documents, 1 feature.
plot(topfeatures(myDfmPct), type="b",
       +      xlab="Top Ten Words", ylab="Percentage of Full Text", xaxt ="n")
axis(1, 1:10, labels = names(topfeatures(myDfmPct)))

## removing stopwords before constructing ngrams
tokensAll <- tokenize(toLower(testText), removePunct = TRUE)
tokensNoStopwords <- removeFeatures(tokensAll, stopwords("english"))
tokensNgramsNoStopwords <- ngrams(tokensNoStopwords, 2)
features(dfm(tokensNgramsNoStopwords) # could be @Dimnames$features
 
length(unique(tokenize(toLower(txt), removePunct = TRUE, simplify = TRUE)))         
         
# TODO: See trim() to shrink dfm size
# Note: Use of nfeature
#freqTerms <- topfeatures(fomcDfm, n = nfeature(fomcDfm))
#freqTerms[freqTerms >= 2]

demo(quanteda)
collocations(x,size=2:3)
dz@Dimnames$features
str(dz)
ntoken(toLower(txt)) 
ntype(toLower(txt)) 
docfreq(dz[,1:20])
# https://cran.r-project.org/web/packages/quanteda/vignettes/LitVignette.html
# next line is all grams
plot(topfeatures(dz, 100), log = "y", cex = .6, ylab = "Term frequency")

fe <- grep("^loving_.*", features(dz)[1:50])
docfreq(dz[,fe])

# loving my ?
docfreq(dz[,grep("^my$", features(dz))]) # unigram
docfreq(dz[,grep("^my_[^_]*$", features(dz))]) #bigrams
docfreq(dz[,grep("^loving_my_[^_]*$", features(dz))]) # trigrams
# get a count like so
docfreq(dz[,grep("^my_[^_]*$", features(dz))])[["my_ac"]] 
# or
unname(docfreq(dz[,grep("^my_[^_]*$", features(dz))])[["my_ac"]])

sort(docfreq(dz[,grep("^my_[^_]*$", features(dz))]), decreasing = TRUE) #bigrams
sort(docfreq(dz[,grep("^loving_my_[^_]*$", features(dz))], decreasing = TRUE) # trigrams

sent <- tokenize(c("Kurt Vongeut said; only assholes use semi-colons.", 
                  +            "Today is Thursday in Canberra:  It is yesterday in London.", 
                          +            "En el caso de que no puedas ir con ellos, ¿quieres ir con nosotros?"), 
                        +          what = "sentence")
paste("<S>", sent[2]) # could do this for all sentences?
# Do we need a sentence ending replacement? maybe so. regex swap sentence endings .!? with </S>
paste("<S>", gsub(pattern = "[.!?]$", replacement = " </S>", sent[2]))
sapply(sent, function(x) paste("<S> ", gsub(pattern = "[.!?]$", replacement = " </S>", x)))
# Do something similar for <UNK>?

# this allows us to keep the b/e markers as single units
# See underscore mention here: http://stackoverflow.com/questions/29541179/text-analysis-using-custom-keywords-in-r
t <- sapply(sent, function(x) paste("_S_ ", gsub(pattern = "[.!?]$", replacement = " _S_", x)))

# combine corpuses
# read in each corpus separately, directly into quanteda
mycorpus1 <- corpus(textfile("UKDraftScouting.csv", textField = "Report"))
mycorpus2 <- corpus(textfile("SECMinusUKDraftScouting.csv", textField = "Report"))
# assign docset variables to each corpus as appropriate 
docvars(mycorpus1, "docset") <- 1 
docvars(mycorpus2, "docset") <- 2
myCombinedCorpus <- mycorpus1 + mycorpus2

# From http://programmers.stackexchange.com/questions/148350/what-algorithms-can-be-used-to-achieve-reasonably-good-next-word-prediction
# Once you have the list of all 3-grams together with their probability you can filter
# your list to all 3-grams starting with "I am". Then you sort all this list by probability 
# et voilà: your prediction.
# another post: um this is literally a Markov Chain... not only that, but literally the most textbook example ... – Justin L. Jun 26 '13 at 6:28 

# Hidden Markov Model (HMM) in R: https://cran.r-project.org/web/packages/HMM/HMM.pdf

# ngram package in R which basically does what we are looking for: https://cran.r-project.org/web/packages/ngram/vignettes/ngram-guide.pdf
# that is, it will generate the next word probabilities

# ngram chapter from jurafky et al: http://web.mit.edu/6.863/www/fall2012/readings/ngrampages.pdf
# very helpful
# probabilities of ngrams: MLE
# look at good-turing for smoothing

# Good explanation of a # of points: http://nlp.stanford.edu/fsnlp/statest/henke-ch6.ppt

# SLM: Statistical Language Modeling
# http://homepages.inf.ed.ac.uk/lzhang10/slm.html

# wordcloud function
#http://www.rdatamining.com/examples/text-mining
#http://www.r-bloggers.com/vectorisation-is-your-best-friend-replacing-many-elements-in-a-character-vector/

http://trinker.github.io/qdap/rm_stopwords.html
http://trinker.github.io/qdap/scrubber.html
http://trinker.github.io/qdap/ngrams.html
http://trinker.github.io/qdap/exclude.html
http://trinker.github.io/qdap/vignettes/qdap_vignette.html#exclude

http://zipfr.r-forge.r-project.org/
http://elib.uni-stuttgart.de/opus/volltexte/2005/2371/

!!! https://github.com/johnmyleswhite/TextRegression  
  
Resources:
http://www.cs.cmu.edu/~biglou/resources/bad-words.txt
http://norvig.com/spell-correct.html
http://rpubs.com/lmullen/nlp-chapter # Annotations might be important here as they break these up into sentences?
https://class.coursera.org/nlp/lecture
https://web.stanford.edu/~jurafsky/NLPCourseraSlides.html

http://www.jstatsoft.org/article/view/v025i05 | Text mining infrastructure in R

http://cs.stanford.edu/people/eroberts/courses/soco/projects/2004-05/nlp/techniques_word.html | Word Prediction
http://www.modsimworld.org/papers/2015/Natural_Language_Processing.pdf | Natural Language Processing: A Model to Predict a Sequence of Words (coursera output)

https://github.com/jan-san/dsci-benchmark | Next word prediction benchmark
https://github.com/ds-capstone-survival-guide/ds-capstone-survival-guide | DS Capstone Survival Guide

### THIS IS VERY HELPFUL ###
http://www.cs.cmu.edu/~shilpaa/sachin_shilpa_cbwptl.pdf | Context Based Word Prediction for Texting Language [uses POS to deal with homonyms, etc]