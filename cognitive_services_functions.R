#' @title Sentiment analysis of climate change communications in the Northern Gulf of California
#' @description Functions to get data
#' @details Functions used on sentiment_climate_change.Rmd
#' @author Hem Nalini Morzaria-Luna hmorzarialuna@gmail.com
#' @date October 2019

We processed articles to separate sentences and applied Microsoft's Cognitive Services using Azure Sentiment Analysis API to do sentiment analysis by sentences. This model uses a combination of techniques during text analysis. Techniques include text processing, part-of-speech analysis, word placement, and word associations. 

```{r}

# article.num <- 1:nrow(article.texts)
# 
# sentence.list <- lapply(article.num, get_sentences, article.texts)
# 
# sentence.tbl <- sentence.list %>% 
#   bind_rows %>% 
#   mutate(sentence_index = 1:nrow(.))
# 
# sentence.length <- 1:nrow(sentence.tbl)
# 
#  #language codes are https://docs.microsoft.com/en-us/azure/cognitive-services/translator/language-support
#  
# # Create the empty environment to store the key
# envCogServices <- new.env(parent = emptyenv())
# 
# 
# cogAuth("bb4ac84fcd9e4b909577882b6eb0b545")

#Scores range from 0 (negative) to 1 (positive)
#Code from
#https://detroitdatalab.com/2018/04/08/sentiment-analysis-in-power-bi-with-microsoft-cognitive-services/?utm_campaign=News&utm_medium=Community&utm_source=DataCamp.com
#https://docs.microsoft.com/en-us/azure/cognitive-services/text-analytics/how-tos/text-analytics-how-to-sentiment-analysis
#sentiment.scores <- lapply(sentence.length,fnCogServicesBatch,phrase.language = "es", endpoint = "sentiment", region = "southcentralus", sentence.tbl)

# sentiment.cog.table.1 <- sentiment.scores %>% 
#                        bind_rows %>% 
#   filter(!is.na(sentiment_score)) %>% 
#                       left_join(sentence.tbl, by ="sentence_index")
# 
# write_csv(sentiment.cog.table.1,"sentence_cogsentiment.csv")

sentiment.cog.table.1 <- read_csv("sentence_cogsentiment.csv")

print(head(sentiment.cog.table.1))
  
```

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