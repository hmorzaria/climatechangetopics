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


get_sentiment <- function(word.tbl,sent.lexicon){
  
  sentiment.tokens <- word.tbl %>% 
  left_join(sent.lexicon, by="word") %>% 
    filter(!is.na(category))
  
  return(sentiment.tokens)
  
}

get_topic <- function(word.tbl,no_topics,top_topics){
  
  term.matrix <- word.tbl %>% 
    group_by(article_index, word) %>% 
    dplyr::summarise(n_cat = n()) %>% 
    cast_dtm(article_index,word,n_cat)
  
  climate.lda <- LDA(term.matrix, k=no_topics, control = list(seed=1234))
  
  climate.topics <- tidy(climate.lda, matrix = "beta")
  
  climate.top.terms <- climate.topics %>% 
    group_by(topic) %>% 
    top_n(top_topics,beta) %>% 
    ungroup() %>% 
    arrange(topic, -beta)
  
  topic.list <- list("term_matrix" = term.matrix, "lda_model" = climate.lda, "lda_terms" = climate.top.terms)
  
  return(topic.list)
}

