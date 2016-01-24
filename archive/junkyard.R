setClass("Node",
         slots = list(ng = "character", p = "numeric", snodes = "hash"))
makeTrie <- function(ng, ngs, count = 1) {
  ngramsLength = length(ngs)
  p = ngs[[count]][[ng]] 
  sbMultiplier <- ngramsLength - count
  node <- new("Node", ng = ng, p = p)
  if (sbMultiplier != 0)
    p <- p * sbMultiplier * .4
  if (count == ngramsLength) {
    #print(paste("LEAF!!!!!", ng))
    #new("Node", p = p, hash(list('a' = 1)))
    node
  } else {
    #print(keys(ngs[[c + 1]]))
    ks <- grep(paste0("^", quotemeta(ng), "_"), keys(ngs[[count + 1]]), value = TRUE, perl = TRUE)
    #print(paste("key:", ng, ", key count:", length(ks)))
    
    #print(paste("ng:", ng, ", ks:", ks, ", hs:", hs))
    #print(paste("ng:", ng, ", ks:", length(ks), ", hs:", length(hs)))
    if (length(ks) == 0) {
      print("ks == 0!")
      #node
      node
    } else {
      r <- sapply(ks,
                  function(k) makeTrie(k, 
                                       ngs, 
                                       count + 1))
      hs <- hash(keys = names(r), values = unname(r))
      #print(unname(r))
      # hs <- hash(keys = ks, values = sapply(ks,
      #                function(k) makeTrie(k, 
      #                                     ngs, 
      #                                     count + 1)))
      #print(keys(hs))
      #print(values(hs))
      #print(paste("Length of returned nodes:", length(hs)))
      #if (length(hs) == 1) {}
      #  hs$nonce <- new("Node")
      #new("Node", p = p, snodes = hash(hs))
      node@snodes <- hs
      node
    }
  }
} # NOTE! Maybe store key names of ngrams rather than another hashmap and look up in global hashmap?
makeNgramTrie <- function(ngs, count = 1) hash(sapply(keys(ngs[[count]]), function(k) makeTrie(k, ngs, count)))

calcPercTrie <- function(ngram, h, count = 1) {
  localNgram <- ngramHead(ngram, count)
  nextNgram <- ngramHead(ngram, count + 1)
  print(h[[localNgram]])
  if (has.key(nextNgram, h[[localNgram]]@snodes)) {
    # get the match values for this level and keep going
    list(localNgram = h[[localNgram]]@p, calcPercTrie(ngram, h[[localNgram]]@snodes, count + 1))
  } else {
    list(localNgram = h[[localNgram]]@p)
  }
}
