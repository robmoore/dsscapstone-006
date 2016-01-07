download.maybe <- function(url, refetch=FALSE, path=".") {
  dest <- file.path(path, basename(url))
  if (refetch || !file.exists(dest))
    download.file(url, dest)
  dest
}

# Faster lookups on vectors
"%fin%" <- function(x, table) fmatch(x, table, nomatch = 0) > 0

makeCluster.default <- function() makeCluster(detectCores() * .75, type="FORK") # FORK will not work on Windows
