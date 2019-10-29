#' @title Sentiment analysis of climate change communications in the Northern Gulf of California
#' @description Functions to get data
#' @details Functions used on sentiment_climate_change.Rmd
#' @author Hem Nalini Morzaria-Luna hmorzarialuna@gmail.com
#' @date September 2019


get_articles <- function(eachlink){
  
  print(paste("Analyzing",eachlink))
  
  thisdomain <- eachlink %>% 
    str_split(pattern="//") %>% 
    unlist %>% 
    .[2] %>% 
    str_split(pattern="/") %>% 
    unlist %>% 
    .[1]
  
  thissite <- eachlink %>% 
    str_split(pattern="/") %>% 
    unlist() %>% 
    .[c(1,2,3)] %>% 
    paste(collapse="/") %>% 
    paste("/",sep="")
  
  #rtxt <- robotstxt(domain=thisdomain)
  
  check.delay <- 10
  
 thissource <- source.list %>% 
    filter(LINK_DEL_PERIODICO==thissite) 
  
  
 text.link <- tryCatch(
    eachlink %>% 
      read_html(encoding=thissource$ENCODING, verbose = TRUE) %>% 
      html_nodes(css=thissource$SELECTOR) %>% 
      html_text() %>% 
      repair_encoding() %>% 
      paste(collapse=" "), 
    error = function(e){NA}    # a function that returns NA regardless of what it's passed
  )
  
  
  html.texts <- tibble(link=eachlink, text=text.link, title=thissource$LINK_DEL_PERIODICO)

  print("Introducing crawl delay")
  Sys.sleep(check.delay)
  
  return(html.texts)
  
}

get.xml <- function(xmlfile, sentiment){
  
  xml.lexicon <- read_xml(xmlfile) %>% 
    xml_find_all(paste("//",sentiment,sep="")) %>% 
    xml_text %>% 
    trimws %>% 
    str_split("  ") %>% 
    unlist() 
  
  xml.lexicon.sent <- tibble(word=xml.lexicon, category=sentiment)
  
  return(xml.lexicon.sent)
}




clean_html <- function(file,random,additional){
  
  article.texts <- read_csv("html_text.csv") %>% 
    filter(!is.na(text)) %>% 
    filter(!grepl("http://www.tribuna.com.mx",link)) %>% 
    filter(!grepl("http://www.informador.com.mx",link)) %>% 
    mutate(text = gsub("\r\n\t\t\t\t\t\t\t\r\n\t\t\t\t\t\t\t\t","",text)) %>% 
    mutate(text = trimws(text)) %>% 
    dplyr::select(text) %>%
    bind_rows(additional) %>% 
    distinct(text)
  

    return(article.texts)
}

get_sentences <- function(eacharticle,article.texts){
  
  print(eacharticle)
  print(article.texts[eacharticle,])
  
  this.text <- article.texts[eacharticle,] %>% 
    pull(text)
  
sentence.list  <- tokenize_sentences(this.text, lowercase = FALSE, strip_punct = TRUE,simplify = FALSE)
  
sentence.text <- sentence.list %>% 
  unlist %>% 
  tibble(sentence=.) %>% 
  mutate(article_index=eacharticle) 

}
  
clean_sentences <- function(sentence.tbl){
  
  random.segments <- c("Fuente Twitter", "Grimm y Enrique R","Redman Nancy B","CIUDAD DE MÉXICO México ene",
                       "No contestó 28 y 13 dijo no sé","Con un hola","Con información de EFE Reuters y El País",
                       "Twitter oppenheimera","Con información de EFE","Sí","Créanme","Con información de AP",
                       "Traducción Esteban Flamini Jeffrey D","Jeffrey D","Con información de EFE","Con información de El País",
                       "Con información de EFE AP y Reuters","Cut the Rope","Line","Sim City","Con información de Azucena Vásquez",
                       "SergioSarmiento","Con información de RT","José María Luis Mora","	Traducción Grupo Reforma",
                       "Con información de Xinhua","Judith D","George W","otoñoinvierno","Rodolfo G","Con información de Esther Díaz",
                       "Un Periodista o un crítico")
  
  sentence.tbl %>% 
    filter(sentence%in% random.segments)
    
}
  

get_words <- function(eacharticle,article.texts,random){
  
  print(eacharticle)
  print(article.texts[eacharticle,])
  
  this.text <- article.texts[eacharticle,]
  
  random.words <- read_csv(random) %>% 
    mutate(word = tolower(word), word = trimws(word)) %>% 
    distinct(word) %>%
    pull(word)
  
  token.text <- this.text %>% 
    unnest_tokens(word, text) %>% 
    mutate(article_index=eacharticle) %>% 
    filter(!word %in% random.words) %>% 
    filter(!grepl("[0-9]+",word)) 

  return(token.text)
}

get_wordcloud <- function(word.tbl){

  word.cloud <- word.tbl %>% 
    group_by(word) %>% 
    dplyr::summarise(n_cat = n()) %>% 
    with(wordcloud(word,n_cat,max.words=75)) 
  
return(word.cloud)
  
}


get_sentiment <- function(word.tbl,sent.lexicon){
  
  sentiment.tokens <- word.tbl %>% 
  left_join(sent.lexicon, by="word") %>% 
    filter(!is.na(category))
  
  return(sentiment.tokens)
  
}

get_sentiment_barplot <- function(sentiment.tokens){

sentiment.barplot <- sentiment.tokens %>% 
    group_by(article_index, category) %>% 
    dplyr::summarise(n_cat = n()) %>% 
    # count(word, category, sort = TRUE) %>% 
    ggplot(aes(category,n_cat, fill = category))+
    geom_boxplot(show.legend = FALSE)+
    ggtitle("Sentiment analysis") +
    xlab("Category") + 
    ylab("No. words")
  
return(sesentiment.barplot)
  
}

get_sentiment_wordcloud <- function(sentiment.tokens){
  
  sentiment.cloud <- sentiment.tokens %>% 
    group_by(word, category) %>% 
    dplyr::summarise(n_cat = n()) %>% 
    acast(word ~ category, value.var = "n_cat", fill = 0) %>% 
    comparison.cloud(colors = c("blue","red"),
                     max.words = 100)
  
  return(sentiment.cloud)
  
}
get_topic <- function(article.texts,no_topics,stop.words){
  
 term.matrix <- article.texts %>% 
    mutate(article_index=1:nrow(.))

  # create a document term matrix 
dtm <- CreateDtm(doc_vec = term.matrix$text, # character vector of documents
                   doc_names = term.matrix$article_index, # document names
                   ngram_window = c(1, 2), # minimum and maximum n-gram length
                   stopword_vec = stop.words, # this is the default value
                   lower = TRUE, # lowercase - this is the default value
                   remove_punctuation = TRUE, # punctuation - this is the default
                   remove_numbers = TRUE, # numbers - this is the default
                   verbose = FALSE, # Turn off status bar for this demo
                   cpus = 4) # default is all available cpus on the system
  
#simplify matrix
  dtm <- dtm[,colSums(dtm)>2]
  
  set.seed(12345)
  
  model <- FitLdaModel(dtm = dtm, 
                       k = no_topics,
                       iterations = 1000, # I usually recommend at least 500 iterations or more
                       burnin = 180,
                       alpha = 0.1,
                       beta = 0.05,
                       optimize_alpha = TRUE,
                       calc_likelihood = TRUE,
                       calc_coherence = TRUE,
                       calc_r2 = TRUE,
                       cpus = 4)
  
 
  # log Likelihood (does not consider the prior) 
  likelihood.plot<- model$log_likelihood %>% 
    ggplot(aes(x=iteration, y=log_likelihood, group=1)) +
    geom_line()+
    ggtitle("") +
    xlab("Interation") + 
    ylab("Log likelihood")+
    ggthemes::theme_clean()
  
  # probabilistic coherence, a measure of topic quality
  # this measure can be used with any topic model, not just probabilistic ones
  
  model$top_terms <- GetTopTerms(phi = model$phi, M = 10)
  
  # Get the prevalence of each topic
  # You can make this discrete by applying a threshold, say 0.05, for
  # topics in/out of docuemnts. 
  model$prevalence <- colSums(model$theta) / sum(model$theta) * 100
  
  # prevalence should be proportional to alpha
  
prevalence.plot <- tibble(prevalence=model$prevalence, alpha=model$alpha) %>% 
    ggplot(aes(alpha,prevalence))+
    geom_point(show.legend = FALSE)+
    xlab("Alpha") + 
    ylab("Prevalence")+
    ggthemes::theme_clean()


  # textmineR has a naive topic labeling tool based on probable bigrams
  model$labels <- LabelTopics(assignments = model$theta > 0.05, 
                              dtm = dtm,
                              M = 1)
  
  # put them together, with coherence into a summary table
  model$summary <- data.frame(topic = rownames(model$phi),
                              label = model$labels,
                              coherence = round(model$coherence, 3),
                              prevalence = round(model$prevalence,3),
                              top_terms = apply(model$top_terms, 2, function(x){
                                paste(x, collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)
  
  
  hist.coherence <- hist(model$coherence, 
       col= "blue", 
       main = "Histogram of probabilistic coherence")
  
  ggplot(data=chol, aes(chol$AGE)) + 
    geom_histogram()
  
  #Histogram of probabilistic coherence
  hist.coherence <- tibble(coherence=model$coherence, index = 1:length(model$coherence)) %>% 
    ggplot(aes(coherence))+
    geom_histogram(binwidth = 0.1,fill="darkblue")+
    xlab("Coherence") + 
    ylab("Frequency")+
    ggthemes::theme_clean()
  
      
  lda.results <- list("model"=model,"likelihood.plot"=likelihood.plot, "prevalence.plot"=prevalence.plot, "hist.coherence"=hist.coherence)

  return(lda.results)
}





predict_lda <- function(model.lda.results,new.text){
  
  new_dtm <- CreateDtm(doc_vec = new.text$text, # character vector of documents
                   doc_names = new.text$article_index, # document names
                   ngram_window = c(1, 2), # minimum and maximum n-gram length
                   stopword_vec = stop.words, # this is the default value
                   lower = TRUE, # lowercase - this is the default value
                   remove_punctuation = TRUE, # punctuation - this is the default
                   remove_numbers = TRUE, # numbers - this is the default
                   verbose = FALSE, # Turn off status bar for this demo
                   cpus = 4) # default is all available cpus on the system
  
  #simplify matrix
  new_dtm <- new_dtm[,colSums(new_dtm)>2]
  
  
  # predictions with gibbs
  assignments <- predict(model.lda.results$model, new_dtm,
                         method = "gibbs", 
                         iterations = 500,
                         burnin = 180,
                         cpus = 2)
  
  
}
