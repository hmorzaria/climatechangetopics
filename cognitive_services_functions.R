#' @title Sentiment analysis of climate change communications in the Northern Gulf of California
#' @description Functions to get data
#' @details Functions used on sentiment_climate_change.Rmd
#' @author Hem Nalini Morzaria-Luna hmorzarialuna@gmail.com
#' @date October 2019



cogAuth <- function(key) {
  
  # Access key assignment for use in REST calls
  assign("keyCogServices", key, envir = envCogServices)
  
}

# Function for using Cognitive Services API
# Note: Can ONLY be used with keyPhrases OR sentiment
fnCogServicesBatch <- function(thesesentences, phrase.language, endpoint, region, sentence.tbl) {
  
  print(thesesentences)
  
  Sys.sleep(30)
  
  # Coerce to character
  text.inputs <- sentence.tbl[thesesentences,] %>% 
  pull(sentence) 
  
  print(text.inputs)
  
  # The URL for Key Phrases cognitive service
  url.cog.service <- paste("https://", region, ".api.cognitive.microsoft.com/text/analytics/v2.0/", endpoint, sep = "")
  
 
  # Create empty list in proper structure for request
  list.docs <- list(documents = list(list()))
  num.max <- length(text.inputs)
  
  # For loop (unfortunately); 
  for (i in 1:num.max) {
    list.docs$documents[[i]] <- list(language = phrase.language, id = i, text = text.inputs[i])
  }
  
  # Convert the list to JSON for posting
  json.body <- toJSON(list.docs)
  
  # Post the call to the REST API
  raw.response <- POST(url.cog.service, add_headers(.headers = c("Ocp-Apim-Subscription-Key" = envCogServices$keyCogServices, "Content-Type" = "application/json")), body = json.body)
 
   # Read in the response as character
  json.response <- readBin(raw.response$content, "character")
  
  print(json.response)
  # Convert the character, now JSON, response back to a list
  list.response <- fromJSON(json.response)
  
  # Extract the first element of each of these
  list.phrases <- lapply(list.response$documents, "[[", 2)
  
  # Unlist to flatten all topics (does this break with score?)
  vec.words <- unlist(list.phrases)
  
  print(vec.words)
  
  if(is.null(vec.words)) stop(return(sentiment.score.tbl))
  
  sentiment.score.tbl <- tibble(sentiment_score=vec.words,sentence_index = thesesentences)

  return(sentiment.score.tbl)
  
}