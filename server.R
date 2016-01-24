library(shiny)

source("common.R")

loadModel <- function(typeName) {
  db <- dbInit(modelName(typeName))
  env <- new.env()
  dbLoad(db, env, c('model'))
  env$model
}

tweetsModel <- loadModel('tweets')
newsModel <- loadModel('news')

if (! exists("makeSuggestions")) {
  makeSuggestions <- memoise(function(q, model) {
    print(paste0("Predicting for '", q, "'"))
    if (nchar(trimws(q)) == 0) {
      suggestions <- list()
    } else {
      q <- removeUnknownFromSentence(q)
      q <- cleanQuery(q, 3)
      # Fill out query to 3 tokens if less
      tokenCount <- stri_count_regex(q, "([^_]+)")
      if (tokenCount < 3) q <- paste0(paste0(replicate(3 - tokenCount, "_S_"), collapse = "_"), "_", q)
      print(paste("Modified query:", q))
      suggestions <- massageResults(lookupProbs(q, model))
      print(paste("Suggestions:", paste(suggestions, collapse = ",")))
    }
    
    if (length(suggestions) != 0) 
      suggestions %>>% list.map(list(suggestion = w, score = p * 100))
    else
      list()
  })
}

section.1 <- function(phrase) {
	makeSuggestions(phrase, tweetsModel)
}

section.2 <- function(phrase) {
	makeSuggestions(phrase, newsModel)
}

section.3 <- function(phrase) {
	data.frame(suggestion = c('x', 'y', 'z'),
			score = c(99, 66, 33))
}

suggestions <- function(phrase) {
	if (is.null(phrase) || (stri_isempty(phrase))) {
		return (NULL);
	}
	section.1 <- makeSuggestions(phrase, tweetsModel)

	section.2 <- makeSuggestions(phrase, newsModel)

	section.3 <- apply(section.3(phrase), 1,
		function(entry) {list(suggestion = entry[[1]], score = entry[[2]])})

	list(phrase = phrase,
		'section-1-id' = section.1,
		'section-2-id' = section.2,
		'section-3-id' = section.3)
}

updateTypeaheadSuggestions <- function(session, id, suggestions) {
	message <- list(id = id,
		suggestions = suggestions)

	session$sendCustomMessage('typeahead.jsUpdateSuggestions', message)
}

shinyServer(function(input, output, session) {
	data <- reactive({
		phrase <- input$q
		suggestions <- suggestions(phrase)

		list(suggestions = suggestions)
	})

	observe({
		if (is.null(data()) || is.null(data()$suggestions)) {
			return
		}
		updateTypeaheadSuggestions(session, 'q', data()$suggestions)
	})
})
