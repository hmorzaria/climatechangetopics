#' @title Sentiment analysis of climate change communications in the Northern Gulf of California
#' @description Code to harves news articles
#' @details Install of package topicmodels requires GNU Scientific Library (GSL) sudo apt-get install gsl-bin libgsl-dev
#' @author Hem Nalini Morzaria-Luna hmorzarialuna@gmail.com
#' @date July 2019

#https://rdrr.io/cran/textmineR/man/predict.lda_topic_model.html
# https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html

# load packages
# List of packages for session


#install.packages("corpus.JSS.papers", repos = "http://datacube.wu.ac.at/", type = "source")

.packages = c(
  "RCurl", "XML","rvest", "httr", "tidyverse","data.table","parallel","doSNOW","here", "tidytext", 
  "wordcloud","tm", "reshape2","topicmodels","textmineR")


# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if (length(.packages[!.inst]) > 0)
  install.packages(.packages[!.inst], dependencies = TRUE)

# Load packages into session
lapply(.packages, require, character.only = TRUE)


data_path <- here()

setwd(data_path)

html.list <- fread("list_links.csv", sep=",", header=TRUE, encoding = "Latin-1") %>% tbl_df()

source.list <- fread("list_sources.csv", sep=",", header=TRUE) %>% tbl_df()

sent.lexicon.1 <- fread("SEL_full.txt", encoding = "Latin-1") %>% 
  dplyr::rename(word = Palabra, category = Categoria)

sent.lexicon.temp <- fread("negative_words_es.txt", header= TRUE, encoding = "UTF-8") %>% 
  tbl_df %>% 
  mutate(category="negative")

sent.lexicon.2 <- fread("positive_words_es.txt", header= TRUE, encoding = "UTF-8")%>% 
  tbl_df %>% 
  mutate(category="positive") %>% 
  bind_rows(sent.lexicon.temp)

spanish.words <- stopwords("spanish") %>% 
  tbl_df %>% 
  setNames("word")

stop.words <- fread("stopwords_spanish.txt", header= TRUE, encoding = "Latin-1") %>% 
  bind_rows(spanish.words) %>% 
  distinct(word)



link.list <- source.list %>% right_join(html_list, by= "NOMBRE DEL PERIODICO") %>% 
  filter(`NOMBRE DEL PERIODICO`!="EXPRESO") 

link.text <- html.list %>% 
  filter(NOMBRE_DEL_PERIODICO %in% c("EL MURAL","REFORMA","LA SILLA ROTA","DIARIO DE ACAYUCÁN","EXPRESO")) %>% 
  mutate(article_num = 1:nrow(.))

article.num <- 1:nrow(link.text)

get_words <- function(eacharticle){
  
  print(eacharticle)
  print(link.text[eacharticle,])
  
  link.text.sent2 <- link.text[eacharticle,] %>% 
    tbl_df %>% 
    unnest_tokens(word, ARTICULO) %>% 
    left_join(sent.lexicon.2, by="word") %>% 
    filter(!is.na(category))
}

word.list <- lapply(article.num, get_words)

word.tbl <- word.list %>% 
  bind_rows %>% 
  anti_join(stop.words, by = "word")
  

#graphs

word.tbl %>% 
  group_by(article_num, category) %>% 
  dplyr::summarise(n_cat = n()) %>% 
  # count(word, category, sort = TRUE) %>% 
  ggplot(aes(category,n_cat, fill = category))+
  geom_boxplot(show.legend = FALSE)+
  ggtitle("Sentiment analysis") +
  xlab("Category") + 
  ylab("No articles")
  
#wordcloud

word.tbl %>% 
  group_by(word, category) %>% 
  dplyr::summarise(n_cat = n()) %>% 
  with(wordcloud(word,n_cat,max.words=50))

#
word.tbl %>% 
  group_by(word, category) %>% 
  dplyr::summarise(n_cat = n()) %>% 
  acast(word ~ category, value.var = "n_cat", fill = 0) %>% 
  comparison.cloud(colors = c("blue","red"),
                   max.words = 50)


#Topic modeling

term.matrix <- word.tbl %>% 
  group_by(article_num, word) %>% 
  dplyr::summarise(n_cat = n()) %>% 
  cast_dtm(article_num,word,n_cat)

climate.lda <- LDA(term.matrix, k=4, control = list(seed=1234))

climate.topics <- tidy(climate.lda, matrix = "beta")

climate.top.terms <- climate.topics %>% 
  group_by(topic) %>% 
  top_n(15,beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

climate.top.terms %>% 
  mutate(term=reorder(term, beta)) %>% 
  ggplot(aes(term,beta, fill = factor(topic)))+
  geom_col()+
  facet_wrap(~ topic, scales = "free")+
  coord_flip()+
  ggtitle("Common terms per topic") +
  xlab("Beta") + 
  ylab("Term")+ 
  labs(fill = "Topic")

beta.spread <- climate.topics %>% 
  mutate(topic = paste0("topic", topic)) %>% 
  spread(topic, beta) %>% 
  filter(topic1 > 0.001 | topic2 > 0.001) %>% 
  mutate(log_ratio = log2(topic2 / topic1))

beta.spread %>% 
  group_by(term) %>% 
  top_n(10,log_ratio) %>% 
  ungroup %>% 
  arrange(term, -log_ratio) %>% 
  ggplot(aes(term,log_ratio))+
  geom_col()+
  coord_flip()


new.topics <- posterior(climate.lda,new.data)

###Apr 25
#Save complete list of newspaper names, states and links, 
# this list will have name of html codes
#merge with Link list
# use it to get selector name for each link using dplyr instead of grepl

# Calculate the number of cores
NumberOfCluster <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)
oldlength <- 1:nrow(link_list)

point.data <- foreach(this.id = oldlength, .verbose=TRUE) %dopar% {
  
  .packages = c(
    "RCurl", "XML","rvest", "httr", "tidyr","data.table","parallel","doSNOW"
  )
  
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  
  this.data <- link_list[this.id,]
  link <- this.data %>% dplyr::select(`LINK NOTA`) %>% as.character()
  html.doc <- read_html(link, encoding="UTF-8", verbose = TRUE)
  selector <- this.data %>% dplyr::select(SELECTOR) %>% as.character()
  source_name <- this.data %>% dplyr::select(`NOMBRE DEL PERIODICO`) %>% as.character()
  
  text_x <- html.doc %>% 
    html_node(selector) %>%
    html_text()
  
 text_x_rep <- repair_encoding(text_x)
  
}

stopCluster(cl)
# download html
